namespace Biz.Morsink.Components.Interop
open Biz.Morsink.Components
open System
open System.Runtime.CompilerServices
open System.Threading.Tasks;
open Container

[<Extension>]
module ContainerExt = 
  [<assembly: Extension>]
  do ()
  
  [<Struct>]
  type ContainerExecutor<'T>(cont:IContainer)=
      [<CompiledName "Execute">]
      member this.aggregate seed (agg:Func<'S, 'T, 'S>) = 
        cont |> fold seed (fun x y -> agg.Invoke (x,y))

      [<CompiledName "Execute">]
      member this.execute (action:Func<'T, 'U>) =
        cont |> map action.Invoke |> Seq.ofList

      [<CompiledName "Execute">]
      member this.execute (action:Action<'T>) =
        cont |> map action.Invoke |> ignore
      
      [<CompiledName "Execute">]
      member this.execute (action:Func<'T, Task<'U>>) =
        cont |> mapAsync (action.Invoke >> Async.AwaitTask) |> Async.StartAsTask

      [<CompiledName "Execute">]
      member this.executeAndAggregate (action:Func<'T,'U>) seed (agg:Func<'S,'U,'S>) = 
        cont |> mapAndFold seed action.Invoke (fun x y -> agg.Invoke (x,y))

      [<CompiledName "TryPick">]
      member this.tryPick (f:Func<'T, bool * 'U>, res:outref<'U>) =
        let result = cont |> tryPick (fun x -> 
            match f.Invoke x with
            | true, v -> Some v
            | _ -> None )
        res <- 
            match result with 
            | Some x -> x
            | None -> null
        result |> Option.isSome

  [<Extension>]
  [<CompiledName "On">]
  let on<'T> cont = ContainerExecutor<'T>(cont)

  