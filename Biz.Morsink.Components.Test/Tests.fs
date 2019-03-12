namespace Biz.Morsink.Components.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Biz.Morsink.Components
open Container


module ContainerTestHelper =
    let private subtypeCount<'a> container = container |> fold (fun x (_:'a) -> x+1) 0
    type ITest =
        abstract member Test : unit -> string
    type TestImpl() =
        member this.Test() = "Test"
        interface ITest with
            member this.Test() = this.Test()

    let ``Registration should be found`` container = 
        let lst = container |> add (TestImpl()) |> build |> getAll<ITest> |> Seq.toList
        Assert.AreEqual (1, lst |> List.length)
        Assert.AreEqual ("Test", lst |> List.head |> (fun x -> x.Test()))

    let ``Multiple registrations allowed`` container =
        let lst = [1..10] |> List.fold (fun st x -> st |> add (TestImpl())) container |> build |> getAll<ITest> |> Seq.toList
        Assert.AreEqual (10, lst |> List.length)

    let ``All subtypes should be found`` container = 
        let container = container |> add "Test" |> build 
        
        Assert.AreEqual (1, subtypeCount<string> container)
        Assert.AreEqual (1, subtypeCount<seq<char>> container)
        Assert.AreEqual (1, subtypeCount<obj> container)
        Assert.AreEqual (1, subtypeCount<IEquatable<string>> container)

    let ``Non-subtypes should not be found`` container = 
        let container = container |> add "Test" |> build 
        
        Assert.AreEqual (0, subtypeCount<char[]> container)
        Assert.AreEqual (0, subtypeCount<ITest> container)

    let ``Recursive object depth 2`` container shouldSucceed =
        let inner = simple |> add (TestImpl()) |> build
        let container = container |> add inner |> add (TestImpl()) |> build
        let num = if shouldSucceed then 2 else 1
        Assert.AreEqual (num, container |> getAll<ITest> |> Seq.length)    
    
    let ``Recursive object depth 3`` container shouldSucceed =
        let inner = simple |> add (TestImpl()) |> build
        let inner2 = container |> add inner |> add (TestImpl()) |> build
        let container = container |> add inner2 |> add (TestImpl()) |> build
        let num = if shouldSucceed then 3 else 1
        Assert.AreEqual (num, container |> getAll<ITest> |> Seq.length)

open ContainerTestHelper

[<AbstractClass>]
type AbstractContainerTest<'c when 'c :> IContainer> (builder: IContainerBuilder<'c>) = 
    [<TestMethod>]
    member this.``Registration should be found`` () =
        ``Registration should be found`` builder

    [<TestMethod>]
    member this.``Multiple registrations allowed`` () = 
        ``Multiple registrations allowed`` builder

    [<TestMethod>]
    member this.``All subtypes should be found`` () = 
        ``All subtypes should be found`` builder
        
    [<TestMethod>]
    member this.``Non-subtypes should not be found`` () = 
        ``Non-subtypes should not be found`` builder
    

[<TestClass>]
type SimpleContainerTest () =
    inherit AbstractContainerTest<IContainer>(simple)
    [<TestMethod>]
    member this.``Recursive call should not work`` () = 
        ``Recursive object depth 2`` simple false
        ``Recursive object depth 3`` simple false

[<TestClass>]
type SimpleConcurrentContainerTest () =
    inherit AbstractContainerTest<IContainer>(simpleConcurrent)    
    [<TestMethod>]
    member this.``Recursive call should not work`` () = 
        ``Recursive object depth 2`` simpleConcurrent false
        ``Recursive object depth 3`` simpleConcurrent false

[<TestClass>]
type RecursiveContainerTest () =
    inherit AbstractContainerTest<IContainer>(recursive)
    [<TestMethod>]
    member this.``Recursive call should work`` () =
        ``Recursive object depth 2`` recursive true
        ``Recursive object depth 3`` recursive true

[<TestClass>]
type RecursiveConcurrentContainerTest () =
    inherit AbstractContainerTest<IContainer>(recursiveConcurrent)
    [<TestMethod>]
    member this.``Recursive call should work`` () =
        ``Recursive object depth 2`` recursiveConcurrent true
        ``Recursive object depth 3`` recursiveConcurrent true

[<TestClass>]
type FlexibleContainerTest () =
    inherit AbstractContainerTest<IFlexibleContainer>(flexible)
    [<TestMethod>]
    member this.``Recursive call should work`` () =
        ``Recursive object depth 2`` recursiveConcurrent true
        ``Recursive object depth 3`` recursiveConcurrent true
    [<TestMethod>]
    member this.``Additions should be returned`` () = 
        let container = flexible |> build
        Assert.AreEqual (0, container |> count<string>)
        let container = container.Add "abc"
        Assert.AreEqual (1, container |> count<string>)
        Assert.AreEqual ("abc", container |> getAll<string> |> Seq.head)
    [<TestMethod>]
    member this.``Removals should not be returned`` () =
        let container = flexible |> add "abc" |> build
        Assert.AreEqual (1, container |> count<string>)
        let container = container.Remove "abc" 
        Assert.AreEqual (0, container |> count<string>)

