namespace Biz.Morsink.Components
open System
open System.Runtime.CompilerServices
open System.Collections.Immutable

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Container =
    open System.Collections.Immutable

    let add (o:obj) (cb:IContainerBuilder<'c>) = cb.Add o
    let build (cb: IContainerBuilder<'c>) = cb.Build ()
    let getAll<'a> (cont: IContainer) = cont.GetAll<'a>()
    let map (action:'a -> 'b) cont = cont |> getAll<'a> |> Seq.map action |> Seq.toList
    let mapAsync (action: 'a -> 'b Async) cont = cont |> getAll<'a> |> Seq.map action |> Async.Parallel
    let fold agg (seed:'s) cont = cont |> getAll<'a> |> Seq.fold agg seed
    let mapAndFold (seed: 's) (action:'a -> 'b) (agg: 's -> 'b -> 's) cont = cont |> getAll<'a> |> Seq.map action |> Seq.fold agg seed 
    let count<'a> cont = cont |> getAll<'a> |> Seq.length
    let tryPick f cont = cont |> getAll |> Seq.tryPick f
    let pick f cont = cont |> tryPick f |> Option.get 
    type SimpleContainerBuilder<'c when 'c :> IContainer>(objects: obj list, construct: obj list -> 'c) =
        static member Empty construct = SimpleContainerBuilder<'c>([], construct)
        interface IContainerBuilder<'c> with
            member this.Add o = SimpleContainerBuilder<'c>(o::objects, construct) :> IContainerBuilder<'c>
            member this.Build () = 
                let res = objects |> construct
                do objects |> List.ofType<IContainerAware> |> Seq.iter (fun ca -> ca.SetContainer res)
                res
    
    type private SimpleContainer(objects : obj []) =
        let mutable indexed = TypeKeyedLookup.Empty
        interface IContainer with
            member this.GetAll<'a>() =
                match indexed.TryGet<'a>() with
                | (true, xs) -> xs
                | (false, _) -> 
                  let filtered = objects |> Seq.ofType<'a>
                  indexed <- indexed.Add filtered
                  filtered
            member this.GetAll (ty:Type) =
                match indexed.TryGet ty with
                | (true,lst) -> lst
                | (false, _) -> 
                  let filtered = objects |> Seq.filter ty.IsInstanceOfType
                  indexed <- indexed.Add (ty, filtered)
                  filtered
    type private SimpleConcurrentContainer (objects: obj []) =
        let indexed = LockFreeTypeKeyedLookup.Empty ()
        interface IContainer with
            member this.GetAll<'a>() =
                match indexed.TryGet<'a>() with
                | (true, xs) -> xs
                | (false, _) -> 
                  let filtered = objects |> Seq.ofType<'a>
                  indexed.Add filtered
                  filtered
            member this.GetAll (ty:Type) =
                match indexed.TryGet ty with
                | (true,lst) -> lst
                | (false, _) -> 
                  let filtered = objects |> Seq.filter ty.IsInstanceOfType
                  indexed.Add (ty, filtered)
                  filtered
    type private RecursiveSimpleContainer (objects: obj []) =
        let mutable indexed = TypeKeyedLookup.Empty
        interface IContainer with
            member this.GetAll<'a>() =
                match indexed.TryGet<'a>() with
                | (true, xs) -> xs
                | (false, _) -> 
                  let deep = objects |> Seq.ofType<IContainer> |> Seq.map getAll<'a>
                  let shallow = objects |> Seq.ofType<'a>
                  let filtered = seq { yield shallow; yield! deep } |> Seq.concat
                  indexed <- indexed.Add filtered
                  filtered
            member this.GetAll (ty:Type) =
                match indexed.TryGet ty with
                | (true,lst) -> lst
                | (false, _) -> 
                  let deep = objects |> Seq.ofType<IContainer> |> Seq.map (fun cont -> cont.GetAll ty)
                  let shallow = objects |> Seq.filter ty.IsInstanceOfType
                  let filtered = seq { yield shallow; yield! deep } |> Seq.concat
                  indexed <- indexed.Add (ty, filtered)
                  filtered    
    type private RecursiveConcurrentContainer (objects: obj []) =
        let indexed = LockFreeTypeKeyedLookup.Empty ()
        interface IContainer with
            member this.GetAll<'a>() =
                match indexed.TryGet<'a>() with
                | (true, xs) -> xs
                | (false, _) -> 
                  let deep = objects |> Seq.ofType<IContainer> |> Seq.map getAll<'a>
                  let shallow = objects |> Seq.ofType<'a>
                  let filtered = seq { yield shallow; yield! deep } |> Seq.concat
                  indexed.Add filtered
                  filtered
            member this.GetAll (ty:Type) =
                match indexed.TryGet ty with
                | (true,lst) -> lst
                | (false, _) -> 
                  let deep = objects |> Seq.ofType<IContainer> |> Seq.map (fun cont -> cont.GetAll ty)
                  let shallow = objects |> Seq.filter ty.IsInstanceOfType
                  let filtered = seq { yield shallow; yield! deep } |> Seq.concat
                  indexed.Add (ty, filtered)
                  filtered
    type private FlexibleContainer (objects: ImmutableHashSet<obj>) =
        let indexed = LockFreeTypeKeyedLookup.Empty ()
        interface IContainer with 
            member this.GetAll<'a> () = 
                match indexed.TryGet<'a>() with
                | (true, lst) -> lst
                | (false, _) -> 
                  let deep = objects |> Seq.ofType<IContainer> |> Seq.map getAll<'a>
                  let shallow = objects |> Seq.ofType<'a>
                  let filtered = seq { yield shallow; yield! deep } |> Seq.concat
                  indexed.Add filtered
                  filtered
            member this.GetAll (ty:Type) = 
                match indexed.TryGet ty with
                | (true,lst) -> lst
                | (false, _) -> 
                  let deep = objects |> Seq.ofType<IContainer> |> Seq.map (fun cont -> cont.GetAll ty)
                  let shallow = objects |> Seq.filter ty.IsInstanceOfType
                  let filtered = seq { yield shallow; yield! deep } |> Seq.concat
                  indexed.Add (ty, filtered)
                  filtered
        interface IFlexibleContainer with
            member this.Add o = objects.Add(o) |> FlexibleContainer :> IFlexibleContainer
            member this.Remove o = objects.Remove(o) |> FlexibleContainer :> IFlexibleContainer

    let internal builder (container: obj list -> 'c) = SimpleContainerBuilder.Empty (fun os -> os |> container) :> IContainerBuilder<'c>

    [<CompiledName("Simple")>]
    let simple = builder (fun objs -> objs |> Array.ofList |> SimpleContainer :> IContainer)
    [<CompiledName("SimpleConcurrent")>]
    let simpleConcurrent = builder (fun objs -> objs |> Array.ofList |> SimpleConcurrentContainer :> IContainer)
    [<CompiledName("Recursive")>]
    let recursive = builder (fun objs -> objs |> Array.ofList |> RecursiveSimpleContainer :> IContainer)
    [<CompiledName("RecursiveConcurrent")>]
    let recursiveConcurrent = builder (fun objs -> objs |> Array.ofList |> RecursiveConcurrentContainer :> IContainer)
    [<CompiledName("Flexible")>]
    let flexible = builder (fun objs -> objs |> Array.ofList |> ImmutableHashSet.Create<obj> |> FlexibleContainer :> IFlexibleContainer)
