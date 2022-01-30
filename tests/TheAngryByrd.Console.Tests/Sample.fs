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
        printfn "%A" query
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

  ]
