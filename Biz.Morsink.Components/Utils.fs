namespace Biz.Morsink.Components
open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Threading

module Seq =
  let ofType<'a> (lst : obj seq) = 
    lst |> Seq.filter (fun x -> x :? 'a) |> Seq.cast<'a>
module List =
  let box lst = lst |> List.map box
  let ofType<'a> (lst : obj list) =
    lst |> Seq.ofType<'a> |> List.ofSeq   

type TypeKeyedLookup private (dict : ImmutableDictionary<Type, obj>) =
    let makeTypedArray (typ:Type) objseq = 
        let oarr = 
            objseq 
            |> Seq.filter typ.IsInstanceOfType
            |> Seq.toArray
        let res = Activator.CreateInstance (typ.MakeArrayType(), oarr.Length) :?> System.Collections.IList
        oarr |> Array.iteri (fun idx elem -> res.[idx] <- elem)
        res :> obj
    
    static member Empty = TypeKeyedLookup(ImmutableDictionary<_,_>.Empty)
    member this.Add (objects: 'a seq) = dict.Add (typeof<'a>, objects |> Seq.cache) |> TypeKeyedLookup
    member this.Add (ty: Type, objects: obj seq) = 
        let arr = objects |> makeTypedArray ty
        dict.Add (ty, arr) |> TypeKeyedLookup
    member this.TryGet<'a> () =
        match dict.TryGetValue typeof<'a> with
        | (true, xs) -> 
            match xs with
            | :? ('a seq) as ys -> (true, ys)
            | _ -> (true, Seq.empty)
        | _ -> (false, Seq.empty)
    member this.Get<'a> () = this.TryGet<'a>() |> snd
    member this.GetAndRemove<'a> () = this.Get<'a>(), this.Remove<'a>()
    member this.Remove<'a> () = dict.Remove typeof<'a> |> TypeKeyedLookup
    member this.Keys = dict.Keys
    member this.Remove (ty: Type) = dict.Remove ty |> TypeKeyedLookup
    member this.TryGet (ty:Type) =
        match dict.TryGetValue ty with
        | (true,xs) -> 
            match xs with
            | :? (obj seq) as ys -> true, ys
            | _ -> true, Seq.empty
        | _ -> false, Seq.empty

type LockFreeTypeKeyedLookup private () =
    let inner = ref TypeKeyedLookup.Empty
    let rec mutate f = 
        let old = !inner
        let nw = f old
        let exch = Interlocked.CompareExchange(inner,nw,old)
        if obj.ReferenceEquals (exch,old) |> not then mutate f // if no success, try again...
    static member Empty () = LockFreeTypeKeyedLookup()
    member this.Add (objects:'a seq) = mutate (fun i -> i.Add objects)
    member this.Add (ty:Type, objects: obj seq) = mutate (fun i -> i.Add (ty,objects))
    member this.TryGet<'a> () = (!inner).TryGet<'a> ()
    member this.Get<'a> () = (!inner).Get<'a> ()
    member this.Remove<'a> () = mutate (fun i -> i.Remove<'a> ())
    member this.Keys = (!inner).Keys
    member this.Remove (ty: Type) = mutate (fun i -> i.Remove ty)
    member this.TryGet (ty: Type) = (!inner).TryGet ty
    member this.TypeKeyedLookup = !inner



