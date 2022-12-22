module QSearch.Types
open System
open System.IO

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

type Paths = private Paths of string[]
    
module Paths =
    let create paths = paths |> Array.map Path.create
    
    let create_from_path (Path path) = Paths [|path|] 
    
    let value (Paths paths) = paths
        
type Strings = private Strings of string[]

module Strings =
    let validate str = if String.IsNullOrEmpty str then Failure "String is null or empty" else Success str
    
    let create strings = strings |> Array.map trim |> Array.map validate
    
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
    Patters: Strings
    Word: string
    ExcludedDirs: Strings
    // ExcludedPathWords: Strings
    SearchOptions: SearchOptions
}

type SearchMessage =
    | Search of SearchParams
    | Quit

type Match = {Path: string; Value: string; Index: int}
    
type Matches = seq<Match> 

