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

type Paths = private Paths of string[]
    
module Paths =
    let validate path = if Directory.Exists path then Success path else Failure $"Directory doesn't exist {path}" 
    
    let create paths = paths |> Array.map trim |> Array.map validate
        
type Strings = private Strings of string[]

module Strings =
    let validate str = if String.IsNullOrEmpty str then Failure "String is null or empty" else Success str
    
    let create strings = strings |> Array.map trim |> Array.map validate
        
(*********************************************************************************************************************  
    Search options
**********************************************************************************************************************)

[<Flags>]
type SearchMode =
    | None = 0
    | CaseSensitive = 1
    | WholeWords = 2
    | RegExp = 4

type SearchOptions = {
    Paths: Paths
    // Patters: Patters
    Word: string
    ExcludedDirs: string[]
    ExcludedPathWords: string[]
    SearchMode: SearchMode
}

