namespace Biz.Morsink.Components
open System

type IContainer =
    abstract GetAll<'a> : unit -> 'a seq
    abstract GetAll : Type -> obj seq
type IContainerBuilder<'c when 'c :> IContainer> =
    abstract Add : obj -> IContainerBuilder<'c>
    abstract Build: unit -> 'c
type IContainerAware =
    abstract SetContainer : IContainer -> unit
type IFlexibleContainer =
    inherit IContainer
    abstract Add : obj -> IFlexibleContainer
    abstract Remove : obj -> IFlexibleContainer
