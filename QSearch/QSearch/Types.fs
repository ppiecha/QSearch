module QSearch.Types
open System
open System.IO
open System.Threading

(*********************************************************************************************************************  
    Nullable
**********************************************************************************************************************)

let nullableToOption (n : System.Nullable<_>) = 
   if n.HasValue 
   then Some n.Value 
   else None
   
let isNullableBoolTrue (b: System.Nullable<bool>) =
    if b.HasValue then b.Value = true else false

(*********************************************************************************************************************  
    Railway oriented programming
**********************************************************************************************************************)

type Result<'TSuccess,'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure      

module Result = 
    let bind switchFunction twoTrackInput =
        match twoTrackInput with
        | Success s -> switchFunction s
        | Failure f -> Failure f

    // convert a normal function into a two-track function
    let map oneTrackFunction twoTrackInput =
        match twoTrackInput with
        | Success s -> Success (oneTrackFunction s)
        | Failure f -> Failure f

    let tee f x =
        f x |> ignore
        x

    let tryCatch f exHandler x =
        try
            f x |> Success
        with
        | ex -> ex |> exHandler |> Failure
    
    let successes (results: Result<'TSuccess,'TFailure>[]) =
        results
        |> Array.choose (function Success s -> Some(s) | Failure f -> None)   
        
    let failures (results: Result<'TSuccess,'TFailure>[]) =
        results
        |> Array.choose (function Failure f -> Some(f) | Success s -> None)
        
    let validate (results: Result<'TSuccess,'TFailure>[]) =
         match failures results with
            | arr when arr |> Array.isEmpty |> not -> Failure arr
            | _ -> Success (successes results)
            

(* use case

let useCase =
    validate1
    >> bind validate2
    >> bind validate3
    >> map canonicalizeEmail
    >> map (tee updateDatabase)
    
let useCase =
    validate1
    >> bind validate2
    >> bind validate3
    >> map canonicalizeEmail
    >> map (tryCatch (tee updateDatabase))    
    
*)

(*********************************************************************************************************************  
    Functions
**********************************************************************************************************************)

let trim (str: string) = str.Trim()

let split (str: string) = str.Split([|';'|], StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) 

(*********************************************************************************************************************  
    Model Types
**********************************************************************************************************************)

type Path = private Path of string

module Path =
    let validate path = if Directory.Exists path then Success (Path path) else Failure $"Directory doesn't exist {path}"
    
    let create path = path |> trim |> validate
    
    let value (Path path) = path

type Paths = private Paths of Path array
    
module Paths =
    // let create paths = paths |> Array.map Path.create
    
    let init = Paths [||]
    
    let create paths =
        paths
        |> Array.map Path.create
        |> Result.validate
        |> function
            | Success paths -> Success (Paths paths)
            | Failure errors -> Failure errors 
    
    let createFromSSV (ssv: string) = ssv |> split |> create  
    
    let createFromPath path = Paths [|path|] 
    
    let value (Paths paths) = paths |> Array.map Path.value
        
type Strings = private Strings of string[]

module Strings =
    
    let init = Strings [||]
    
    let validate str = if String.IsNullOrEmpty str then Failure "String is null or empty" else Success str
    
    let create strings =
        strings
        |> Array.map trim
        |> Array.map validate
        |> Result.validate
        |> function
            | Success strings -> Success (Strings strings)
            | Failure errors -> Failure errors 
    
    let createFromSSV ssv = ssv |> split |> create
    
    let value (Strings strings) = strings
        
(*********************************************************************************************************************  
    Search options
**********************************************************************************************************************)

[<Flags>]
type SearchOptions =
    | None = 0
    | CaseSensitive = 1
    | WholeWords = 2
    | RegExp = 4

type SearchParams = {
    Paths: Paths
    Patterns: Strings
    Word: string
    ExcludedDirs: Strings
    // ExcludedPathWords: Strings
    SearchOptions: SearchOptions
}

type SearchMessage = SearchParams * AsyncReplyChannel<CancellationTokenSource> * SynchronizationContext

type Match = {Value: string; Index: int}
    
type FileMatches = {FileName: string; Matches: seq<Match>}

type FileContent = private {charArray: char array}

module FileContent =
    let create (fileContent: string) = {
        charArray = Seq.toArray fileContent
    }
    
    let is_end_of_line c = c = '\010'
    
    let lineStart (fileContent: inref<FileContent>) (index: int) =
        let before_index = Seq.toArray fileContent.charArray[..(index - 1)]
        Array.tryFindIndexBack is_end_of_line before_index
    
    let lineEnd (fileContent: inref<FileContent>) (index: int) =
        let before_index = Seq.toArray fileContent.charArray[..(index - 1)]
        let after_index = Seq.toArray fileContent.charArray[index..]
        after_index
        |> Array.tryFindIndex is_end_of_line
        |> Option.map (fun x -> x + Array.length before_index)
           
    let lineText (fileContent: inref<FileContent>) (index: int) =
        let start = lineStart &fileContent index
        let end_ = lineEnd &fileContent index
        match start, end_ with
        | None, None -> fileContent.charArray |> String
        | None, Some(e) -> fileContent.charArray[..(e - 1)] |> String
        | Some(s), None -> fileContent.charArray[(s + 1)..] |> String
        | Some(s), Some(e) -> fileContent.charArray[(s + 1)..(e - 1)] |> String
    
    let lineNumber (fileContent: inref<FileContent>) (index: int) =
        let before_index = Seq.toArray fileContent.charArray[..(index - 1)]
        before_index |> Array.filter is_end_of_line |> Array.length |> (+) 1
    
    let indexInLine (fileContent: inref<FileContent>) (index: int) =
        let start = lineStart &fileContent index 
        match start with
        | None -> index
        | Some(start) -> let before_start = Seq.toArray fileContent.charArray[..start] |> Array.length
                         index - before_start
        

type FileResult = {
    FileName: string
    NumberOfMatches: int
}

type FileLineResult = {
    FileName: string
    LineNumber: int
    Value: string
    ValueLength: int
    Index: int
    LineText: string
    IndexInLine: int
}

 
let createFileLineResult fileName (fileContent: inref<FileContent>) (match_: Match) =
    {
        FileName=fileName
        LineNumber=FileContent.lineNumber &fileContent match_.Index
        Value=match_.Value
        ValueLength=String.length match_.Value 
        Index=match_.Index
        LineText=FileContent.lineText &fileContent match_.Index
        IndexInLine=FileContent.indexInLine &fileContent match_.Index
    }
        
        
type FileSearchResult = {
    FileResult: FileResult
    FileLineResults: FileLineResult array
}

type FileSearchResultWithExn =
    | FileSearchResult of FileSearchResult
    | Exn of string
    | NoMatches of string
    with
    static member accept = function
        | FileSearchResult _ | Exn _ -> true
        | NoMatches _ -> false
    
module FileSearchResult =
    let create (fileContent: inref<FileContent>) {FileName=fileName; Matches=matches} =
        let fileLineResults = ResizeArray<FileLineResult>()
        for match_ in matches do
                fileLineResults.Add (createFileLineResult fileName &fileContent match_)
        let fileLineResults = fileLineResults.ToArray()
        let fileResult = {FileName=fileName; NumberOfMatches=Array.length fileLineResults}
        FileSearchResult {FileResult=fileResult; FileLineResults=fileLineResults}
        
        
        
    
