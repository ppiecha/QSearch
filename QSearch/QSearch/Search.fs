module QSearch.Search

open System
open System.IO
open System.Text.RegularExpressions
open QSearch.Types
open System.Threading


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


let findMatchesInFile word (searchOptions: SearchOptions) (fileContent:string) = 
    let pattern = if searchOptions.HasFlag SearchOptions.WholeWords then @"\b" + word + @"\b" else word
    let regexOptions = RegexOptions.Compiled
    let ignoreCase = not (searchOptions.HasFlag SearchOptions.CaseSensitive)
    let regexOptions = if ignoreCase then regexOptions ||| RegexOptions.IgnoreCase else regexOptions
    let rx = Regex(pattern, regexOptions)
    rx.Matches(fileContent) |> Seq.map (fun m -> {Value = m.Value; Index = m.Index})   


let planFileMatches searchParams fileName = async {
    let! fileContent = readFile fileName
    let matches = 
        match String.IsNullOrEmpty searchParams.Word with
        | false -> findMatchesInFile searchParams.Word searchParams.SearchOptions fileContent
        | true -> Seq.empty
    let fileContent = FileContent.create fileContent
    return FileSearchResult.create &fileContent {FileName=fileName; Matches=matches} 
    // return {FileName=fileName; Matches=matches}
}


let planAllFilesMatches (searchParams: SearchParams) =
    let paths = Paths.value searchParams.Paths
    let patterns = Strings.value searchParams.Patterns
    let excludedDirs = Strings.value searchParams.ExcludedDirs
    getAllFilesByPatternList paths patterns excludedDirs
    |> Seq.map (fun fileName -> planFileMatches searchParams fileName |> Async.Catch) 
    |> Async.Parallel
    

let searchEvent = Event<Choice<FileSearchResult, exn>[]>()
let searchFinished = searchEvent.Publish

(*********************************************************************************************************************  
    Search processor
**********************************************************************************************************************)

let searcher =
    MailboxProcessor<SearchMessage>.Start(fun inbox ->
        let rec loop () = async {
            let! searchParams, replyChannel, ctx = inbox.Receive()
            let cts = new CancellationTokenSource()
            try
                replyChannel.Reply(cts)
                let results = Async.RunSynchronously ((planAllFilesMatches searchParams), -1, cts.Token)
                do! Async.SwitchToContext ctx
                searchEvent.Trigger results
            with :? OperationCanceledException -> searchEvent.Trigger [||]
            do! loop ()
            // | Cancel ->
            //     do! loop ()
            // | Quit -> return ()
        }
        loop ())

