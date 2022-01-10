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

module Option =
    let inline ofNull value =
        if Object.ReferenceEquals(value, null) then Some value
        else None 
    let inline bindNull ([<InlineIfLambda>] binder) option =
        match option with
        | Some x -> binder x |> ofNull
        | None -> None

[<Interface>] type IProvideLoggers = abstract CreateLogger: categoryName:string -> ILogger

[<Interface>] type IProvideConfiguration = abstract Configuration: IConfiguration

[<Interface>] 
type IWrapDapper =
    abstract QueryAsync<'T> : sql: string * ?param:obj * ?transaction:IDbTransaction * ?commandTimeout:int * ?commandType:CommandType -> Task<'T seq>
    abstract ExecuteAsync : sql: string * ?param:obj * ?transaction:IDbTransaction * ?commandTimeout:int * ?commandType:CommandType -> Task<int>
    abstract SelectAsync<'T> : sql: SelectQuery * ?transaction:IDbTransaction * ?commandTimeout:int  -> Task<'T seq>
    abstract InsertAsync<'T> : sql: InsertQuery<'T> * ?transaction:IDbTransaction * ?commandTimeout:int  -> Task<int>
    abstract UpdateAsync<'T> : sql: UpdateQuery<'T> * ?transaction:IDbTransaction * ?commandTimeout:int  -> Task<int>
    abstract DeleteAsync : sql: DeleteQuery * ?transaction:IDbTransaction * ?commandTimeout:int  -> Task<int>

[<Interface>] type IProvideDatabaseAccess = abstract Database: IWrapDapper

[<Interface>] 
type IEnvironment =
    inherit IProvideLoggers
    inherit IProvideConfiguration
    inherit IProvideDatabaseAccess

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

// module Db = 
//     let fetchUser (env: #IDb) userId = 
//         env.Database.Query("", {| userId = userId |})
//     let updateUser (env: #IDb) user = env.Database.Execute("", user)

module App =
    open FsToolkit.ErrorHandling

    [<CLIMutable>]
    type User = {
        Name : string
        Email : string
    }

    let fetchUsers (env: #IProvideDatabaseAccess) = async {
        // conn.ExecuteAsync("")
        return! env.Database.QueryAsync<User>("SELECT * FROM users") |> Async.AwaitTask
        // return! Db.fetchUser env "3"
    }

    let doWork env = async {
        let logger = LogProvider.getCategoryNameByFunc env
        let mutable someValue = 42
        Log.info $"Starting work {someValue:anotherValue}" logger
        let config = Configuration.get env
        // printfn "-> %s" config.["lol"]
        let! users = fetchUsers env
        // config.AsEnumerable() |> Seq.iter(printfn "%A")
        do! Async.Sleep 2000

        // failwith "lol"
        someValue <- someValue / 2
        Log.warn $"Finishing work {someValue:anotherValue}" logger
        ()
    }

type AppEnv (service : IServiceProvider) = 
    interface IEnvironment with 
        // why `service.GetService`? It allows us to lazily get the implementation of an interface rather than
        // creating it everytime AppEnv is created. This mimics the same concept when a class will only use certain interfaces
        // so not all things are created everytime, boosting performance. It does add boiler plate but it's not bad.
        
        // IProvideLoggers
        member _.CreateLogger name = service.GetService<ILoggerProvider>().CreateLogger(name)
        // IProvideConfiguration
        member _.Configuration = service.GetService<IConfiguration>()
        // IProvideDatabaseAccess
        member _.Database = service.GetService<IWrapDapper>()

type DapperWrapper(connection : IDbConnection) =
    interface IWrapDapper with
        member _.QueryAsync<'T>(sql: string, ?param: obj, ?transaction: IDbTransaction, ?commandTimeout: int, ?commandType: CommandType) =
            connection.QueryAsync<'T>(sql, ?param=param, ?transaction = transaction, ?commandTimeout= commandTimeout, ?commandType=commandType)
        member _.ExecuteAsync(sql: string, ?param: obj, ?transaction: IDbTransaction, ?commandTimeout: int, ?commandType: CommandType) =
            connection.ExecuteAsync(sql, ?param=param, ?transaction = transaction, ?commandTimeout= commandTimeout, ?commandType=commandType)
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
        loggingBuilder.AddConsole() |> ignore
        ()

    let configureServices (serviceCollection : IServiceCollection) : unit = 
        let serviceProvider = serviceCollection.BuildServiceProvider()
        // for service in serviceCollection do
        //     let implName =
        //         service.ImplementationType
        //         |> Option.ofObj
        //         |> Option.map (fun t -> t.FullName)
        //         |> Option.defaultValue ""
        //     printfn $"{service.ServiceType.FullName}, {service.Lifetime}, {implName}"
        // let config = serviceCollection.BuildServiceProvider().GetService<IConfiguration>().GetConnectionString
        let builder = NpgsqlConnectionStringBuilder(
            Host = "localhost",
            Database = "ppp"
        )
        serviceCollection.AddTransient<IDbConnection>(fun sp -> new NpgsqlConnection(builder |> string)) |> ignore
        serviceCollection.AddTransient<IWrapDapper,DapperWrapper>() |> ignore
        serviceCollection.AddScoped<IEnvironment, AppEnv>() |> ignore
        ()
    let configureHostConfiguration (builder : IConfigurationBuilder) : unit=
        ()
    let configureAppConfiguration (builder : IConfigurationBuilder) : unit=
        ()
    let configureContainer (builder : HostBuilderContext) (contaier : 'TContainerBuilder) : unit=
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
        let appEnv = host.Services.GetService<IEnvironment>()
        let logger = appEnv |> LogProvider.createLogger "AppDomain.CurrentDomain.UnhandledException" 
        setupUnhandleExcpetionHandling logger

        do! App.doWork appEnv

        return 0
    }

    [<EntryPoint>]
    let main argv =
        use cts = new CancellationTokenSource()
        setupCloseSignalers cts
        Async.RunSynchronously(mainAsync argv, cancellationToken=cts.Token)