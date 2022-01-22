namespace TheAngryByrd.Console

open System
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.EndpointRouting
open System
open System.Data
open Dapper
open Dapper.FSharp
open Npgsql
open Dapper.FSharp.PostgreSQL
open System.Threading.Tasks
open System.Threading
open Microsoft.Extensions.Caching.Distributed
open System.Collections.Generic



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
    abstract QueryAsync<'T> :  sql: string * ?param:IDictionary<string,obj> * ?transaction:IDbTransaction * ?commandTimeout:int * ?commandType:CommandType * ?cancellationToken : CancellationToken -> Task<'T seq>
    abstract QueryIAsync<'T> : sql: FormattableString  * ?transaction:IDbTransaction * ?commandTimeout:int * ?commandType:CommandType * ?cancellationToken : CancellationToken -> Task<'T seq>
    abstract QuerySingleAsync<'T> :  sql: string * ?param:IDictionary<string,obj> * ?transaction:IDbTransaction * ?commandTimeout:int * ?commandType:CommandType * ?cancellationToken : CancellationToken -> Task<'T>
    abstract QuerySingleIAsync<'T> : sql: FormattableString  * ?transaction:IDbTransaction * ?commandTimeout:int * ?commandType:CommandType * ?cancellationToken : CancellationToken -> Task<'T>
    abstract ExecuteAsync :    sql: string * ?param:IDictionary<string,obj> * ?transaction:IDbTransaction * ?commandTimeout:int * ?commandType:CommandType * ?cancellationToken : CancellationToken -> Task<int>
    abstract ExecuteIAsync :   sql: FormattableString  * ?transaction:IDbTransaction * ?commandTimeout:int * ?commandType:CommandType * ?cancellationToken : CancellationToken -> Task<int>
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
        actor_id : int32
        first_name : string
        last_name : string
        last_update : DateTime
    }

    let fetchActorById (actor_id : int) (env: #IProvideDatabaseAccess) = async {
        let! ct = Async.CancellationToken
        // let param = dict [ "@id", box id]
        // return! env.Database.QueryAsync<Actor>("SELECT * FROM actor WHERE actor_id=@id", param = param, cancellationToken=ct) |> Async.AwaitTask
        
        return! env.Database.QuerySingleIAsync<Actor>($"SELECT * FROM actor WHERE actor_id={actor_id}", cancellationToken=ct) |> Async.AwaitTask
    }

open Microsoft.AspNetCore.Http
type RequireEnv() =
    static member services(map: IEnvironment -> HttpFunc -> HttpContext -> Task<HttpContext option>) =
        fun next (ctx : HttpContext) ->
            map (ctx.GetService<IEnvironment>()) next ctx
    static member services(map: IEnvironment -> HttpFunc -> HttpContext -> Async<HttpContext option>) =
        fun next (ctx : HttpContext) ->
            Async.StartAsTask(map (ctx.GetService<IEnvironment>()) next ctx, cancellationToken = ctx.RequestAborted)

module App =
    open FsToolkit.ErrorHandling
    open System.Text.Json



    let getActor actor_id env = async {
        let createCacheKey id = $"postgres:actors:{id}"
        let getActorFromCache actor_id env = asyncResult {
            let logger = LogProvider.getCategoryNameByFunc env
            Log.info $"Fetching actor {actor_id:actor_id} from cache"  logger
            let! ct = Async.CancellationToken
            try 
                let cache = DistributedCache.get env
                let! actorAsJson = cache.GetStringAsync(createCacheKey actor_id, ct) |> Async.AwaitTask
                if isNull actorAsJson then
                    return! Error "Cache miss"
                else
                    return JsonSerializer.Deserialize<ActorRepository.Actor> actorAsJson
            with e ->
                return! Error (e.Message)
        }
        let getActorFromDatabaseThenCache actor_id env error_reason = asyncResult {
            let logger = LogProvider.getCategoryNameByFunc env
            Log.info $"Fetching actor {actor_id:actor_id} from database because cache failed for reason: {error_reason:error_reason}, " logger
            let! ct = Async.CancellationToken
            let! actor = ActorRepository.fetchActorById actor_id env

            let cache = DistributedCache.get env
            let cacheOptions = DistributedCacheEntryOptions(AbsoluteExpirationRelativeToNow = TimeSpan.FromSeconds(10.))
            let actorAsJson = JsonSerializer.Serialize  actor
            do! cache.SetStringAsync(createCacheKey actor_id, actorAsJson, cacheOptions, ct) |> Async.AwaitTask
            return actor
        }
        return!
            getActorFromCache actor_id env
            |> AsyncResult.orElseWith (getActorFromDatabaseThenCache actor_id env)
    }

    // let doWork env = async {
    //     let logger = LogProvider.getCategoryNameByFunc env
    //     let mutable someValue = 42
    //     Log.info $"Starting work {someValue:anotherValue} {DateTimeOffset.utcNow env:now}" logger
    //     let config = Configuration.get env
    //     let actorId = Int32.Parse(config.["ActorToFind"])
    //     let! actor = getActor actorId env
    //     let actorName = $"{actor.first_name} {actor.last_name}"
    //     // config.AsEnumerable() |> Seq.iter(printfn "%A")
    //     do! Async.Sleep 2000
    //     // failwith "lol"
    //     someValue <- someValue /2
    //     Log.warn $"Finishing work {actorId:actorId}, {actorName:actorName}, {someValue:anotherValue}, {DateTimeOffset.utcNow env:now}" logger
    //     ()
    // }

    let getActorById (actor_id : int) (env) (next : HttpFunc) (ctx : HttpContext) = async {
        try 
            match! getActor actor_id env with
            | Ok actor ->
                return! json actor next ctx |> Async.AwaitTask
            | Error e ->
                printfn "%A" e
                return! setStatusCode 404 next ctx |> Async.AwaitTask
        with e ->
            return! setStatusCode 500 next ctx |> Async.AwaitTask
    }

    let endpoints = [
        GET [
            routef "/actors/%i" (
                fun actor_id -> 
                    RequireEnv.services(
                        getActorById actor_id
            ))
        ]
    ]

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


module Sql =
    let queryI (sql: FormattableString)  =
        let mutable parameterizedString = sql.Format
        for i = 0 to sql.ArgumentCount-1 do
            parameterizedString <- parameterizedString.Replace($"{{{i}}}", $"@p{i}")
        let parameters =
            List.init (sql.ArgumentCount) (fun i -> $"p{i}", sql.GetArgument(i))
            |> dict
        parameterizedString, parameters
type DapperWrapper(connection : IDbConnection) =
    interface IWrapDapper with
        member _.QueryAsync<'T>(sql: string, ?param: IDictionary<string,obj>, ?transaction: IDbTransaction, ?commandTimeout: int, ?commandType: CommandType,  ?cancellationToken : CancellationToken) =
            CommandDefinition(commandText = sql, ?parameters = (param |> Option.map box), ?transaction = transaction, ?commandTimeout= commandTimeout, ?commandType=commandType, ?cancellationToken = cancellationToken)
            |> connection.QueryAsync<'T>
        member this.QueryIAsync<'T>(sql: FormattableString, ?transaction: IDbTransaction, ?commandTimeout: int, ?commandType: CommandType,  ?cancellationToken : CancellationToken) =
            let commandText, parameters = Sql.queryI sql
            (this :> IWrapDapper).QueryAsync<'T>(commandText,param = parameters ,?transaction = transaction, ?commandTimeout=commandTimeout, ?commandType=commandType,?cancellationToken = cancellationToken)
        member _.QuerySingleAsync<'T>(sql: string, ?param: IDictionary<string,obj>, ?transaction: IDbTransaction, ?commandTimeout: int, ?commandType: CommandType,  ?cancellationToken : CancellationToken) =
            CommandDefinition(commandText = sql, ?parameters = (param |> Option.map box), ?transaction = transaction, ?commandTimeout= commandTimeout, ?commandType=commandType, ?cancellationToken = cancellationToken)
            |> connection.QuerySingleAsync<'T>
        member this.QuerySingleIAsync<'T>(sql: FormattableString, ?transaction: IDbTransaction, ?commandTimeout: int, ?commandType: CommandType,  ?cancellationToken : CancellationToken) =
            let commandText, parameters = Sql.queryI sql
            (this :> IWrapDapper).QuerySingleAsync<'T>(commandText,param = parameters ,?transaction = transaction, ?commandTimeout=commandTimeout, ?commandType=commandType,?cancellationToken = cancellationToken)

        member _.ExecuteAsync(sql: string, ?param: IDictionary<string,obj>, ?transaction: IDbTransaction, ?commandTimeout: int, ?commandType: CommandType, ?cancellationToken : CancellationToken) =
            CommandDefinition(commandText = sql, ?parameters = (param |> Option.map box), ?transaction = transaction, ?commandTimeout= commandTimeout, ?commandType=commandType, ?cancellationToken = cancellationToken)
            |> connection.ExecuteAsync
        member this.ExecuteIAsync(sql: FormattableString, ?transaction: IDbTransaction, ?commandTimeout: int, ?commandType: CommandType, ?cancellationToken : CancellationToken) =
            let commandText, parameters = Sql.queryI sql
            (this :> IWrapDapper).ExecuteAsync(commandText,param = parameters ,?transaction = transaction, ?commandTimeout=commandTimeout, ?commandType=commandType,?cancellationToken = cancellationToken)

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

    let configureLogging (hostbuilderContext : WebHostBuilderContext) (loggingBuilder : ILoggingBuilder) : unit = 
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

    let configureServices (hostbuilderContext : WebHostBuilderContext) (serviceCollection : IServiceCollection) : unit = 
        serviceCollection.AddGiraffe() |> ignore
        serviceCollection.AddTransient<IDbConnection>(fun sp -> 
            let dvdrentaldb = sp.GetService<IConfiguration>().GetConnectionString("dvdrentaldb")
            new NpgsqlConnection(dvdrentaldb)) |> ignore
        serviceCollection.AddTransient<IWrapDapper,DapperWrapper>() |> ignore
        serviceCollection.AddStackExchangeRedisCache(fun  o ->
            o.Configuration <- hostbuilderContext.Configuration.GetConnectionString("redis")
        ) |> ignore
        serviceCollection.AddTransient<IEnvironment, AppEnvironment>() |> ignore
        
        // printServices serviceCollection
        ()
    let configureHostConfiguration  (builder : IConfigurationBuilder) : unit=
        ()
    let configureAppConfiguration (hostbuilderContext : HostBuilderContext) (builder : IConfigurationBuilder) : unit=
        ()
    let configureContainer  (hostbuilderContext : HostBuilderContext) (container : 'TContainerBuilder) : unit=
        ()
    let configureHostOptions  (hostbuilderContext : HostBuilderContext) (hostOptions : HostOptions) : unit=
        ()
    let configure endpoints (hostbuilderContext : WebHostBuilderContext) (builder : IApplicationBuilder) =   
        builder
            .UseRouting()
            .UseEndpoints(fun e -> e.MapGiraffeEndpoints(endpoints))
        |> ignore
        ()




    let mainAsync (argv : string array) = async {
        do! Async.SwitchToThreadPool()
        let! ct = Async.CancellationToken
        
        let hostBuilder =
            Host
                .CreateDefaultBuilder(argv)
                .ConfigureWebHostDefaults(fun webhost ->
                    webhost
                        .Configure(configure App.endpoints)
                        .ConfigureLogging(configureLogging)
                        .ConfigureServices(configureServices)

                    
                    |> ignore
                )
                // .ConfigureWebHost()
                // .ConfigureLogging(configureLogging)
                // .ConfigureHostConfiguration(configureHostConfiguration)
                // .ConfigureAppConfiguration(configureAppConfiguration)
                // .ConfigureContainer(configureContainer)
                // .ConfigureHostOptions(configureHostOptions)
                // .ConfigureServices(configureServices)
        let host = hostBuilder.Build()
        
        use serviceScope = host.Services.CreateScope()
        let appEnv = serviceScope.ServiceProvider.GetService<IEnvironment>()
        let logger = LogProvider.createLogger "AppDomain.CurrentDomain.UnhandledException" appEnv
        setupUnhandleExcpetionHandling logger
        // while true do 
        //     do! App.doWork appEnv
        // do! App.doWork (host.Services.GetService<_>())
        
        do! host.RunAsync(ct) |> Async.AwaitTask

        return 0
    }

    [<EntryPoint>]
    let main argv =
        use cts = new CancellationTokenSource()
        setupCloseSignalers cts
        Async.RunSynchronously(mainAsync argv, cancellationToken=cts.Token)