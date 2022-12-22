module QSearch.Search

open System.IO
open System.Text.RegularExpressions
open QSearch.Types
open Types


let rec getAllFiles dir pattern =
    seq { yield! Directory.EnumerateFiles(dir, pattern)
          for d in Directory.EnumerateDirectories(dir) do
              yield! getAllFiles d pattern }
    
    
let rec getAllDirs dir =
    seq { for d in Directory.EnumerateDirectories(dir) do
              yield d
              yield! getAllDirs d }


let readLines (filePath:string) = seq {
 use sr = new StreamReader (filePath)
 while not sr.EndOfStream do
     yield sr.ReadLine ()
}


let printTotalFileBytes path =
    async {
        let! bytes = File.ReadAllBytesAsync(path) |> Async.AwaitTask
        let fileName = Path.GetFileName(path)
        printfn $"File {fileName} has %d{bytes.Length} bytes"
    }
    
    
let readFile (path:string) = async {
  use sr = new StreamReader(path)
  return! sr.ReadToEndAsync() |> Async.AwaitTask
}


(*********************************************************************************************************************  
    Main search functions
**********************************************************************************************************************)

let rec getAllFilesByPatternList (paths: string[]) (patterns: string[]) (excludedDirs: string[]) = seq {
    for path in paths do
        for pattern in patterns do
            yield! Directory.EnumerateFiles(path, pattern)
        for dir in Directory.EnumerateDirectories(path) do
            if excludedDirs |> Array.contains dir |> not then
                yield! getAllFilesByPatternList [|dir|] patterns excludedDirs
}   


let findMatchesInFile word (searchOptions: SearchOptions) (fileName:string) = async {
    let! content = readFile fileName
    let pattern = if searchOptions.HasFlag SearchOptions.WholeWords then @"\b" + word + @"\b" else word
    let regexOptions = RegexOptions.Compiled
    let ignoreCase = not (searchOptions.HasFlag SearchOptions.CaseSensitive)
    let regexOptions = if ignoreCase then regexOptions ||| RegexOptions.IgnoreCase else regexOptions
    let rx = Regex(pattern, regexOptions)
    return rx.Matches(content) |> Seq.map (fun m -> {Path = fileName; Value = m.Value; Index = m.Index})   
}


let planAllMatches (searchParams: SearchParams) =
    let paths = Paths.value searchParams.Paths
    let patterns = Strings.value searchParams.Patters
    let excludedDirs = Strings.value searchParams.ExcludedDirs
    getAllFilesByPatternList paths patterns excludedDirs
    |> Seq.map (findMatchesInFile searchParams.Word searchParams.SearchOptions)
    |> Async.Parallel
    

let searchEvent = Event<Matches>()
let searchFinished = searchEvent.Publish

(*********************************************************************************************************************  
    Search processor
**********************************************************************************************************************)

let searcher =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () = async {
            let! message = inbox.Receive()
            match message with
            | Search searchParams ->
                let all_matches = 
                    searchParams
                    |> planAllMatches
                    |> Async.RunSynchronously
                    |> Seq.collect id
                searchEvent.Trigger all_matches
                do! loop ()
            | Quit -> return ()
        }
        loop ())

