module Tests

open Expecto
open TheAngryByrd.Console
open System

type IGetActorRequirements = 
  inherit IProvideLoggers 
  inherit IProvideCaching
  inherit IProvideDatabaseAccess
  inherit IProvideDateTime

module IGetActorRequirements =
  let create createLogger distributedCache database utcNow = 
    {
      new IGetActorRequirements with
        member _.CreateLogger name = createLogger name
        member _.DistributedCache = distributedCache
        member _.Database: IWrapDapper = database
        member _.UtcNow = utcNow ()
    }

module ILogger =
  open Microsoft.Extensions.Logging
  open System

  let create () = {
    new ILogger with
    override this.BeginScope(state: 'TState): System.IDisposable = 
        failwith "Not Implemented"
    override this.IsEnabled(logLevel: LogLevel): bool = 
        false
    override this.Log(logLevel: LogLevel, eventId: EventId, state, ``exception``: exn, formatter: System.Func<_,exn,string>): unit = 
        ()
  }

module IDistributedCache =
  open Microsoft.Extensions.Caching.Distributed
  open System.Threading.Tasks
  let create () = {
    new IDistributedCache with
    override this.Get(key: string): byte[] = 
        Array.zeroCreate 0
    override this.GetAsync(key: string, token: System.Threading.CancellationToken): System.Threading.Tasks.Task<byte[]> = 
        Task.FromResult(Array.zeroCreate 0)
    override this.Refresh(key: string): unit = 
        ()
    override this.RefreshAsync(key: string, token: System.Threading.CancellationToken): System.Threading.Tasks.Task = 
        Task.CompletedTask
    override this.Remove(key: string): unit = 
        ()
    override this.RemoveAsync(key: string, token: System.Threading.CancellationToken): System.Threading.Tasks.Task = 
        Task.CompletedTask
    override this.Set(key: string, value: byte[], options: DistributedCacheEntryOptions): unit = 
        ()
    override this.SetAsync(key: string, value: byte[], options: DistributedCacheEntryOptions, token: System.Threading.CancellationToken): System.Threading.Tasks.Task = 
        Task.CompletedTask
  }

open System.Threading.Tasks
open System
type IIWrapDapper =
  // Passing functions to implement members here doesn't really work. See https://stackoverflow.com/questions/70886932/implementing-an-interface-with-a-generic-parameter-on-a-f-record
  // So this is a limited approach of being able to configure interfaces in tests easily
  static member create (?querySingleIAsync : FormattableString -> Task<obj>) = {
    new IWrapDapper with
    override this.DeleteAsync(sql: Dapper.FSharp.DeleteQuery, transaction: System.Data.IDbTransaction option, commandTimeout: int option, cancellationToken: System.Threading.CancellationToken option): System.Threading.Tasks.Task<int> = 
        failwith "Not Implemented"
    member this.ExecuteAsync(sql: string, param: System.Collections.Generic.IDictionary<string,obj> option, transaction: System.Data.IDbTransaction option, commandTimeout: int option, commandType: System.Data.CommandType option, cancellationToken: System.Threading.CancellationToken option): System.Threading.Tasks.Task<int> = 
        failwith "Not Implemented"
    member this.ExecuteIAsync(sql: System.FormattableString, transaction: System.Data.IDbTransaction option, commandTimeout: int option, commandType: System.Data.CommandType option, cancellationToken: System.Threading.CancellationToken option): System.Threading.Tasks.Task<int> = 
        failwith "Not Implemented"
    member this.InsertAsync<'T>(sql: Dapper.FSharp.InsertQuery<'T>, transaction: System.Data.IDbTransaction option, commandTimeout: int option, cancellationToken: System.Threading.CancellationToken option): System.Threading.Tasks.Task<int> = 
        failwith "Not Implemented"
    member this.QueryAsync<'T>(sql: string, param: System.Collections.Generic.IDictionary<string,obj> option, transaction: System.Data.IDbTransaction option, commandTimeout: int option, commandType: System.Data.CommandType option, cancellationToken: System.Threading.CancellationToken option): System.Threading.Tasks.Task<seq<'T>> = 
        failwith "Not Implemented"
    member this.QueryIAsync<'T>(sql: System.FormattableString, transaction: System.Data.IDbTransaction option, commandTimeout: int option, commandType: System.Data.CommandType option, cancellationToken: System.Threading.CancellationToken option): System.Threading.Tasks.Task<seq<'T>> = 
        failwith "Not Implemented"
    member this.QuerySingleAsync<'T>(sql: string, param: System.Collections.Generic.IDictionary<string,obj> option, transaction: System.Data.IDbTransaction option, commandTimeout: int option, commandType: System.Data.CommandType option, cancellationToken: System.Threading.CancellationToken option): System.Threading.Tasks.Task<'T> = 
        Task.FromResult<'T>(Unchecked.defaultof<'T>)
    member this.QuerySingleIAsync<'T>(sql: System.FormattableString, transaction: System.Data.IDbTransaction option, commandTimeout: int option, commandType: System.Data.CommandType option, cancellationToken: System.Threading.CancellationToken option) : Task<'T> =
      task {
        match querySingleIAsync with
        | Some q ->
          let! x = q sql 
          return downcast x
        | None ->
          return Unchecked.defaultof<'T>
      }
    member this.SelectAsync<'T>(sql: Dapper.FSharp.SelectQuery, transaction: System.Data.IDbTransaction option, commandTimeout: int option, cancellationToken: System.Threading.CancellationToken option): System.Threading.Tasks.Task<seq<'T>> = 
        failwith "Not Implemented"
    member this.UpdateAsync<'T>(sql: Dapper.FSharp.UpdateQuery<'T>, transaction: System.Data.IDbTransaction option, commandTimeout: int option, cancellationToken: System.Threading.CancellationToken option): System.Threading.Tasks.Task<int> = 
        failwith "Not Implemented" 
  }
open Moq
open Microsoft.Extensions.Logging
open TheAngryByrd.Console.ActorRepository
open Microsoft.Extensions.Caching.Distributed
open Microsoft.Extensions.Logging.Abstractions
open Microsoft.Extensions.Caching.StackExchangeRedis
open Microsoft.Extensions.Options
open Npgsql
open DotNet.Testcontainers.Builders
open DotNet.Testcontainers.Containers
open DotNet.Testcontainers.Configurations

[<Tests>]
let tests =
  testList "samples" [
    testCaseAsync "Get Actor With Object Expressions" <| async {
      let logger = fun name -> ILogger.create()
      let distributedCache = IDistributedCache.create()
      let actor : ActorRepository.Actor = {
        first_name = "William"
        actor_id = 1701
        last_name = "Shatner"
        last_update = DateTime.Now
      }
      let expected = Ok actor
      let querySingleIAsync _ = task {
        return box actor
      }
      let database = IIWrapDapper.create(querySingleIAsync)
      let utcNow = fun () -> System.DateTimeOffset.MinValue
      let env = IGetActorRequirements.create logger distributedCache database utcNow

      let! actual = App.getActor 3 env
      Expect.equal actual expected ""
    }
    testCaseAsync "Get Actor With Mocks" <| async {
      let actor : ActorRepository.Actor = {
        first_name = "William"
        actor_id = 1701
        last_name = "Shatner"
        last_update = DateTime.Now
      }
      let expected = Ok actor
      
      let envMock = Mock<IGetActorRequirements>()
      let logger = Mock<ILogger>()
      envMock.Setup(fun env -> env.CreateLogger(It.IsAny<string>())).Returns(logger.Object) |> ignore
      let database = Mock<IWrapDapper>()
      let querySingleIAsync = task {
        return actor
      }
      // F# optional parameters and Moq are weird. You have to specify all named parameters and use the `?paramName` syntax otherwise Moq doesn't recognize it being called
      database.Setup(fun env -> env.QuerySingleIAsync<Actor>(It.IsAny<_>(),?transaction=It.IsAny<_>(),?commandTimeout=It.IsAny<_>(),?commandType=It.IsAny<_>(),?cancellationToken=It.IsAny<_>())).Returns(querySingleIAsync) |> ignore
      envMock.Setup(fun env -> env.Database).Returns(database.Object) |> ignore
      let distributedCache = Mock<IDistributedCache>()
      envMock.Setup(fun env -> env.DistributedCache).Returns(distributedCache.Object) |> ignore

      envMock.Setup(fun env -> env.UtcNow).Returns(DateTime.UtcNow) |> ignore

      let! actual = App.getActor 3 envMock.Object

      Expect.equal actual expected ""
    }
    testCaseAsync "Get Actor Integration" <| async {
      
      // Enabling logging
      // TestcontainersSettings.Logger <- 
      //   let f = LoggerFactory.Create(fun f -> f.AddSimpleConsole().SetMinimumLevel(LogLevel.Trace) |> ignore)
      //   let l = f.CreateLogger("DotNet.TestContainers")
      //   l
      
      let! redisContainerBuilder = 
        async {
          let redisContainerBuilder =
            TestcontainersBuilder<RedisTestcontainer>()
              .WithDatabase(new RedisTestcontainerConfiguration())
          let redisContainer = redisContainerBuilder.Build() 
          do! redisContainer.StartAsync()
          return redisContainer
        } |> Async.StartChild

      let! psqlContainerBuilder = 
        async {
          let psqlContainerBuilder =
            (new TestcontainersBuilder<PostgreSqlTestcontainer>())       
              // WithDatabase doesn't work correctly with this image since it's environment it expects id init for the database
              // https://github.com/kristiandupont/dvdrental-image/blob/master/amd64.dockerfile#L5
              // https://github.com/HofmeisterAn/dotnet-testcontainers/blob/ef67133a1287a5cd183af9ae21e56fd44089f69e/src/DotNet.Testcontainers/Configurations/Modules/Databases/PostgreSqlTestcontainerConfiguration.cs#L35
              // https://github.com/HofmeisterAn/dotnet-testcontainers/blob/ef67133a1287a5cd183af9ae21e56fd44089f69e/src/DotNet.Testcontainers/Builders/TestcontainersBuilderDatabaseExtension.cs#L17-L18
              // Not really sure how postgres hooks all this up as their containers are quite large
              // .WithDatabase(new PostgreSqlTestcontainerConfiguration(Database = "dvdrental", Username="postgres", Password="postgres" ))
              .WithImage("kristiandupont/dvdrental-image")  
              .WithPortBinding(0, 5432)
              .ConfigureContainer(fun c ->
                c.ContainerPort <- 5432
                c.Database <- "dvdrental"
                c.Username <- "postgres"
                c.Password <- "postgres"
              )
              .WithWaitStrategy(Wait.ForUnixContainer().UntilCommandIsCompleted("pg_isready -h 'localhost' -p '5432'"));
          let psqlContainer = psqlContainerBuilder.Build() 

          do! psqlContainer.StartAsync()
          return psqlContainer
        } |> Async.StartChild
      
      use! redisContainer = redisContainerBuilder
      use! psqlContainer = psqlContainerBuilder

      let logger _ = NullLogger.Instance :> ILogger

      let cache = 
        let redisCacheOption = RedisCacheOptions(Configuration = redisContainer.ConnectionString)
        new RedisCache(Options.Create(redisCacheOption)) :> IDistributedCache

      let database =
        let conn = new NpgsqlConnection(psqlContainer.ConnectionString)
        DapperWrapper(conn) :> IWrapDapper

      let utcNow () = DateTimeOffset.UtcNow

      let env = IGetActorRequirements.create logger cache database utcNow

      let expected = 
        Ok
          { 
            actor_id = 3
            first_name = "Ed"
            last_name = "Chase"
            last_update = DateTime.Parse("2013-05-26T14:47:57.6200000")
          }

      let! actual = App.getActor 3 env

      Expect.equal actual expected ""
    }

  ]
