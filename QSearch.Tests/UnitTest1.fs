module QSearch.Tests

open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.Pass()
    
[<Test>]
let TestAdd () =
    let actual = Search.add 2 3
    let expected = 5
    Assert.That(actual, Is.EqualTo(expected))
    

[<TestCase(" ", "")>]    
[<TestCase(" t", "t")>]
[<TestCase(" t ", "t")>]
[<TestCase(" t t ", "t t")>]
[<TestCase("", "")>]
let TestTrim (x: string, y: string) =
    Assert.AreEqual(Types.trim x, y)
    
[<TestCase(" ; t ", [|"t"|])>]    
[<TestCase(" ; t ;", [|"t"|])>]
[<TestCase(" t ;  t;", [|"t"; "t"|])>]
[<TestCase("t; t; s", [|"t"; "t"; "s"|])>]

let TestSplit (x: string, y: string[]) =
    Assert.AreEqual(Types.split x, y)

[<TestCase(";")>]
[<TestCase("")>]    
let TestEmptySplit(x: string) =
    let empty: string[] = Array.empty
    Assert.AreEqual(Types.split x, empty)
    
    