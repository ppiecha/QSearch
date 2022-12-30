module QSearch.Tests

open Microsoft.VisualBasic
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

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
    

let mutable fileContent =
    Types.FileContent.create "t1\nt2\nt3\nt4"


[<TestCase(0)>]
let TestLineStartNull (x: int) =
    Assert.IsNull(Types.FileContent.lineStart &fileContent x)
    
[<TestCase(3, 2)>]
[<TestCase(4, 2)>]
[<TestCase(5, 2)>]
[<TestCase(6, 5)>]
let TestLineStart (x: int, y: int) =
    Assert.AreEqual(Some(y), Types.FileContent.lineStart &fileContent x)
    
[<TestCase(9)>]
let TestLineEndNull (x: int) =
    Assert.IsNull(Types.FileContent.lineEnd &fileContent x)

// "t1\nt2\nt3\nt4"    
[<TestCase(0, 2)>]
[<TestCase(2, 2)>]
[<TestCase(3, 5)>]
[<TestCase(6, 8)>]
let TestLineEnd (x: int, y: int) =
    Assert.AreEqual(Some(y), Types.FileContent.lineEnd &fileContent x)
    
[<TestCase(0, "t1")>]
[<TestCase(1, "t1")>]
[<TestCase(2, "t1")>]
[<TestCase(3, "t2")>]
[<TestCase(4, "t2")>]
[<TestCase(5, "t2")>]
[<TestCase(6, "t3")>]
let TestLineText(x: int, y: string) =
    Assert.AreEqual(y, Types.FileContent.lineText &fileContent x)
    
[<TestCase(0, 1)>]
[<TestCase(1, 1)>]
[<TestCase(2, 1)>]
[<TestCase(3, 2)>]
[<TestCase(4, 2)>]
[<TestCase(5, 2)>]
[<TestCase(6, 3)>]
let TestLineNumber(x: int, y: int) =
    Assert.AreEqual(y, Types.FileContent.lineNumber &fileContent x)

// "t1\nt2\nt3\nt4"      
[<TestCase(0, 0)>]
[<TestCase(1, 1)>]
[<TestCase(2, 2)>]
[<TestCase(3, 0)>]
[<TestCase(4, 1)>]
[<TestCase(5, 2)>]
[<TestCase(6, 0)>]
let TestIndexInLine(x: int, y: int) =
    Assert.AreEqual(y, Types.FileContent.indexInLine &fileContent x)

[<TestCase("*.*", [|"*.*"|])>]    
let TestStrings(x: string, y: string[]) =
    match x |> Types.Strings.createFromSSV with
    | Types.Result.Success strings -> Assert.AreEqual(y, Types.Strings.value strings)
    | Types.Result.Failure errors -> failwith (errors |> String.concat @"\n") 

    
    