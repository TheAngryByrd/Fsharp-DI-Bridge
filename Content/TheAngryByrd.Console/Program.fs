namespace TheAngryByrd.Console

open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Configuration
open System
open System.Data
open Dapper
open Dapper.FSharp
open Npgsql
open Dapper.FSharp.PostgreSQL
open System.Threading.Tasks
open System.Threading
open Microsoft.Extensions.Caching.Distributed

module Option =
    let inline ofNull value =
        if Object.ReferenceEquals(value, null) then None
        else Some value 
    let inline bindNull ([<InlineIfLambda>] binder) option =
        match option with
        | Some x -> binder x |> ofNull
        | None -> None


[<Interface>] type IProvideLoggers = abstract CreateLogger: categoryName:string -> ILogger

[<Interface>] type IProvideConfiguration = abstract Configuration: IConfiguration

[<Interface>] 
type IWrapDapper =
    abstract QueryAsync<'T> :  sql: string * ?param:obj * ?transaction:IDbTransaction * ?commandTimeout:int * ?commandType:CommandType * ?cancellationToken : CancellationToken -> Task<'T seq>
    abstract ExecuteAsync :    sql: string * ?param:obj * ?transaction:IDbTransaction * ?commandTimeout:int * ?commandType:CommandType * ?cancellationToken : CancellationToken -> Task<int>
    abstract SelectAsync<'T> : sql: SelectQuery * ?transaction:IDbTransaction * ?commandTimeout:int  -> Task<'T seq>
    abstract InsertAsync<'T> : sql: InsertQuery<'T> * ?transaction:IDbTransaction * ?commandTimeout:int  -> Task<int>
    abstract UpdateAsync<'T> : sql: UpdateQuery<'T> * ?transaction:IDbTransaction * ?commandTimeout:int  -> Task<int>
    abstract DeleteAsync :     sql: DeleteQuery * ?transaction:IDbTransaction * ?commandTimeout:int  -> Task<int>

[<Interface>] type IProvideDatabaseAccess = abstract Database: IWrapDapper

[<Interface>] 
type IProvideDateTime = 
    abstract UtcNow: DateTimeOffset

[<Interface>] 
type IProvideCaching = 
    abstract DistributedCache: IDistributedCache
[<Interface>] 
type IEnvironment =
    inherit IProvideLoggers
    inherit IProvideConfiguration
    inherit IProvideDatabaseAccess
    inherit IProvideDateTime
    inherit IProvideCaching

module Configuration =
    let get(env: #IProvideConfiguration) = env.Configuration

type LogProvider =
    static member inline createLogger categoryName (env : #IProvideLoggers) = env.CreateLogger categoryName
    static member inline getCategoryNameByFunc (env : #IProvideLoggers, [<System.Runtime.CompilerServices.CallerMemberName>] ?memberName: string) =

        // When we're in a CE we get something like `WebBackend.App+thingsToCall2@130`.
        // CallerMemberName gets us the function that actually called it
        // Splitting off + seems like the best option to get the Fully Qualified Path
        let location = 
            System.Reflection.MethodBase.GetCurrentMethod() |> Option.ofObj
            |> Option.bindNull(fun mb -> mb.DeclaringType)
            |> Option.bindNull(fun dt -> dt.FullName)
            |> Option.bind(fun name -> name.Split('+') |> Seq.tryHead)
            |> Option.defaultValue ""

        LogProvider.createLogger $"{location}.{memberName.Value}" env

module DateTimeOffset =
    let utcNow (env : #IProvideDateTime) = env.UtcNow
module Log =
    open System
    open System.Text.RegularExpressions
    let _formatterRegex =
        Regex(@"(?<!{){(?<number>\d+)(?<columnFormat>:(?<format>[^}]+))?}(?!})", RegexOptions.Compiled)

    let inline _setMessageInterpolated (message: FormattableString) =
            let mutable messageFormat = message.Format

            let args =
                _formatterRegex.Matches(messageFormat)
                |> Seq.cast<Match>
                |> Seq.map
                    (fun m ->
                        let number = Int32.Parse(m.Groups.["number"].Value)
                        let formatGroup = m.Groups.["format"]
                        let propertyValue = message.GetArgument(number)
                        let propertyName = formatGroup.Value
                        let columnFormatGroup = m.Groups.["columnFormat"]
                        propertyName, propertyValue, columnFormatGroup.Index, columnFormatGroup.Length
                        )
            // Reverse the args so we won't change the indexes earlier in the string
            args
            |> Seq.rev
            |> Seq.iter
                (fun (_, _, removeStart, removeLength) ->
                    if removeLength > 0 then
                        messageFormat <- messageFormat.Remove(removeStart, removeLength))

            let namedArgs =
                args
                |> Seq.map (fun (name, _, _, _) -> box $"{{{name}}}")
                |> Seq.toArray

            let argValues =
                args
                |> Seq.map(fun (_,value,_,_) -> value)
                |> Seq.toArray

            messageFormat <-
                messageFormat
                    .Replace("{{", "{{{{")
                    .Replace("}}", "}}}}")
            // Replace numbered args with named args from regex match
            messageFormat <- String.Format(messageFormat, args = namedArgs)
            messageFormat, argValues


    let inline trace message (logger : #ILogger)  =
        let messageFormat, args = _setMessageInterpolated message
        logger.LogTrace(messageFormat, args)
    let inline traceWithExn message (exn : exn) (logger : #ILogger)  =
        let messageFormat, args = _setMessageInterpolated message
        logger.LogTrace(exn, messageFormat, args)

    let inline debug message (logger : #ILogger)  =
        let messageFormat, args = _setMessageInterpolated message
        logger.LogDebug(messageFormat, args)
    let inline debugWithExn message (exn : exn) (logger : #ILogger)  =
        let messageFormat, args = _setMessageInterpolated message
        logger.LogDebug(exn, messageFormat, args)

    let inline info message (logger : #ILogger)  =
        let messageFormat, args = _setMessageInterpolated message
        logger.LogInformation(messageFormat, args)
    let inline infoWithExn message (exn : exn) (logger : #ILogger)  =
        let messageFormat, args = _setMessageInterpolated message
        logger.LogInformation(exn, messageFormat, args)

    let inline warn message (logger : #ILogger)  =
        let messageFormat, args = _setMessageInterpolated message
        logger.LogWarning(messageFormat, args)
    let inline warnWithExn message (exn : exn) (logger : #ILogger)  =
        let messageFormat, args = _setMessageInterpolated message
        logger.LogWarning(exn, messageFormat, args)

    let inline error message (logger : #ILogger)  =
        let messageFormat, args = _setMessageInterpolated message
        logger.LogError(messageFormat, args)
    let inline errorWithExn message (exn : exn) (logger : #ILogger)  =
        let messageFormat, args = _setMessageInterpolated message
        logger.LogError(exn, messageFormat, args)

    let inline critical message (logger : #ILogger)  =
        let messageFormat, args = _setMessageInterpolated message
        logger.LogCritical(messageFormat, args)
    let inline criticalWithExn message (exn : exn) (logger : #ILogger)  =
        let messageFormat, args = _setMessageInterpolated message
        logger.LogCritical(exn, messageFormat, args)

module DistributedCache =
    let get (env : #IProvideCaching) = env.DistributedCache

module ActorRepository =
    [<CLIMutable>]
    type Actor = {
        actor_id : int64
        first_name : string
        last_name : string
        last_update : DateTime
    }

    let fetchActors (env: #IProvideDatabaseAccess) = async {
        let! ct = Async.CancellationToken
        return! env.Database.QueryAsync<Actor>("SELECT * FROM actor", cancellationToken=ct) |> Async.AwaitTask
    }
module App =
    open FsToolkit.ErrorHandling

    let getActorName env = async {
        
        let logger = LogProvider.getCategoryNameByFunc env
        let! ct = Async.CancellationToken
        let cache = DistributedCache.get env
        let! firstActor = cache.GetStringAsync("first-actor", ct) |> Async.AwaitTask
        if isNull firstActor then
            Log.info $"Cache miss, Fetching actors" logger
            let! actors = ActorRepository.fetchActors env
            let actor = actors |> Seq.head
            let name = actor.first_name
            let cacheOptions = DistributedCacheEntryOptions(AbsoluteExpirationRelativeToNow = TimeSpan.FromSeconds(10.))
            do! cache.SetStringAsync("first-actor", name, cacheOptions, ct) |> Async.AwaitTask
            return name
        else
            return firstActor
    }

    let doWork env = async {
        let! ct = Async.CancellationToken
        let logger = LogProvider.getCategoryNameByFunc env
        let mutable someValue = 42
        Log.info $"Starting work {someValue:anotherValue} {DateTimeOffset.utcNow env:now}" logger
        let config = Configuration.get env
        let mycustomKey = config.["MyCustomKey"]
        let! actorName = getActorName env
        // config.AsEnumerable() |> Seq.iter(printfn "%A")
        do! Async.Sleep 2000
        // failwith "lol"
        someValue <- someValue /2
        Log.warn $"Finishing work {mycustomKey:mycustomKey}, {actorName:actorName}, {someValue:anotherValue}, {DateTimeOffset.utcNow env:now}" logger
        ()
    }

type AppEnvironment (service : IServiceProvider) = 
    interface IEnvironment with 
        // why `service.GetService`? It allows us to lazily get the implementation of an interface rather than
        // creating it everytime AppEnv is created. This mimics the same concept when a class will only use certain interfaces
        // so not all things are created everytime, boosting performance. 
        // Even C# is starting to consider lazy instantiation for dependencies in a class https://thecodeblogger.com/2021/04/28/delayed-instantiation-using-dependency-injection-in-net/
        // It does add boiler plate but it's not bad.
        
        // IProvideLoggers
        member _.CreateLogger name = service.GetService<ILoggerProvider>().CreateLogger(name)
        // IProvideConfiguration
        member _.Configuration = service.GetService<IConfiguration>()
        // IProvideDatabaseAccess
        member _.Database = service.GetService<IWrapDapper>()
        // IProvideDateTime
        member _.UtcNow = DateTimeOffset.UtcNow
        // IProvideCaching
        member _.DistributedCache = service.GetService<IDistributedCache>()

type DapperWrapper(connection : IDbConnection) =
    interface IWrapDapper with
        member _.QueryAsync<'T>(sql: string, ?param: obj, ?transaction: IDbTransaction, ?commandTimeout: int, ?commandType: CommandType,  ?cancellationToken : CancellationToken) =
            CommandDefinition(commandText = sql, ?parameters = param, ?transaction = transaction, ?commandTimeout= commandTimeout, ?commandType=commandType, ?cancellationToken = cancellationToken)
            |> connection.QueryAsync<'T>
        member _.ExecuteAsync(sql: string, ?param: obj, ?transaction: IDbTransaction, ?commandTimeout: int, ?commandType: CommandType, ?cancellationToken : CancellationToken) =
            CommandDefinition(commandText = sql, ?parameters = param, ?transaction = transaction, ?commandTimeout= commandTimeout, ?commandType=commandType, ?cancellationToken = cancellationToken)
            |> connection.ExecuteAsync
        member _.SelectAsync<'T>(sql: SelectQuery, ?transaction: IDbTransaction, ?commandTimeout: int) =
            connection.SelectAsync<'T>(sql, ?trans = transaction, ?timeout= commandTimeout)
        member _.InsertAsync<'T>(sql: InsertQuery<'T>, ?transaction: IDbTransaction, ?commandTimeout: int) =
            connection.InsertAsync<'T>(sql, ?trans = transaction, ?timeout= commandTimeout)
        member _.UpdateAsync<'T>(sql: UpdateQuery<'T>, ?transaction: IDbTransaction, ?commandTimeout: int) =
            connection.UpdateAsync<'T>(sql, ?trans = transaction, ?timeout= commandTimeout)
        member _.DeleteAsync(sql: DeleteQuery, ?transaction: IDbTransaction, ?commandTimeout: int) =
            connection.DeleteAsync(sql, ?trans = transaction, ?timeout= commandTimeout)

module Main =
    open System
    open System.Threading

    let inline tryCancel (cts : CancellationTokenSource) =
        try
            cts.Cancel()
        with :? ObjectDisposedException as e -> 
            // if CTS is disposed we're probably exiting cleanly
            ()

    let setupCloseSignalers (cts : CancellationTokenSource) =
        Console.CancelKeyPress.Add(fun _ ->
            // printfn "CancelKeyPress"
            tryCancel cts
        )
        System.Runtime.Loader.AssemblyLoadContext.Default.add_Unloading(fun _ ->
            // printfn "AssemblyLoadContext unload"
            tryCancel cts
        )
        AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
            // printfn "ProcessExit"
            tryCancel cts
        )

    let setupUnhandleExcpetionHandling env =
        AppDomain.CurrentDomain.UnhandledException.Add(fun data -> 
            match data.ExceptionObject with
            | :? exn as ex -> Log.criticalWithExn  $"Unhandled exception. IsTerminating:{data.IsTerminating:IsTerminating}" ex env
            | _ -> ()
            ()
        )

    let configureLogging (loggingBuilder : ILoggingBuilder) : unit = 
        loggingBuilder.ClearProviders() |> ignore
        loggingBuilder.AddSimpleConsole(fun o -> 
            o.SingleLine <- true
            o.UseUtcTimestamp <- true
            o.TimestampFormat <- "yyyy-MM-ddTHH\\:mm\\:ss.fffffffzzz "
            o.IncludeScopes <- true
            ()
        ) |> ignore
        ()
        
    let printServices (serviceCollection : IServiceCollection) =
        for service in serviceCollection do
            let implName =
                service.ImplementationType
                |> Option.ofObj
                |> Option.map (fun t -> t.FullName)
                |> Option.defaultValue ""
            printfn $"{service.ServiceType.FullName}, {service.Lifetime}, {implName}"

    let configureServices (serviceCollection : IServiceCollection) : unit = 
        let serviceProvider = serviceCollection.BuildServiceProvider()

        serviceCollection.AddScoped<IDbConnection>(fun sp -> 
            let dvdrentaldb = sp.GetService<IConfiguration>().GetConnectionString("dvdrentaldb")
            new NpgsqlConnection(dvdrentaldb)) |> ignore
        serviceCollection.AddScoped<IWrapDapper,DapperWrapper>() |> ignore
        serviceCollection.AddStackExchangeRedisCache(fun  o ->
            o.Configuration <- serviceProvider.GetService<IConfiguration>().GetConnectionString("redis")
        ) |> ignore
        serviceCollection.AddScoped<IEnvironment, AppEnvironment>() |> ignore
        
        // printServices serviceCollection
        ()
    let configureHostConfiguration (builder : IConfigurationBuilder) : unit=
        ()
    let configureAppConfiguration (builder : IConfigurationBuilder) : unit=
        ()
    let configureContainer (builder : HostBuilderContext) (container : 'TContainerBuilder) : unit=
        ()
    let configureHostOptions (builder : HostOptions) : unit=
        ()
    let mainAsync (argv : string array) = async {
        do! Async.SwitchToThreadPool()
        let! ct = Async.CancellationToken
        
        let hostBuilder =
            Host
                .CreateDefaultBuilder(argv)
                .ConfigureLogging(configureLogging)
                .ConfigureHostConfiguration(configureHostConfiguration)
                .ConfigureAppConfiguration(configureAppConfiguration)
                .ConfigureContainer(configureContainer)
                .ConfigureHostOptions(configureHostOptions)
                .ConfigureServices(configureServices)
                

        let! host = hostBuilder.StartAsync(ct) |> Async.AwaitTask
        use serviceScope = host.Services.CreateScope()
        let appEnv = serviceScope.ServiceProvider.GetService<IEnvironment>()
        let logger = LogProvider.createLogger "AppDomain.CurrentDomain.UnhandledException" appEnv
        setupUnhandleExcpetionHandling logger
        while true do 
            do! App.doWork appEnv
        // do! App.doWork (host.Services.GetService<_>())

        return 0
    }

    [<EntryPoint>]
    let main argv =
        use cts = new CancellationTokenSource()
        setupCloseSignalers cts
        Async.RunSynchronously(mainAsync argv, cancellationToken=cts.Token)