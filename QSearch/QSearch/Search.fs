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
    try 
        use sr = new StreamReader(path)
        let! content = sr.ReadToEndAsync() |> Async.AwaitTask 
        return Success(content)
    with exn -> return Failure(exn.Message)
}


(*********************************************************************************************************************  
    Main search functions
**********************************************************************************************************************)

// let safeSeq x =
//     try
//         Success(x)
//     with exn -> Failure exn.Message 
//
// let filesGenerator path pattern =
//     fun () -> Directory.EnumerateFiles(path, pattern)
//
let enumerateFiles path pattern =
    try
        let files = Directory.EnumerateFiles(path, pattern)
        seq { for file in files do yield Success file }
    with exn -> seq { Failure exn.Message }  
    
let enumerateDirectories path =
    try
        let dirs = Directory.EnumerateDirectories(path)
        seq { for dir in dirs do yield Success dir }
    with exn -> seq { Failure exn.Message }        

let rec getAllFilesByPatternList (paths: string[]) (patterns: string[]) (excludedDirs: string[]) = seq {
    for path in paths do
        for pattern in patterns do
                yield! enumerateFiles path pattern 
        for dir in enumerateDirectories(path) do
            match dir with
            | Success dir -> if excludedDirs |> Array.contains dir |> not then
                                 yield! getAllFilesByPatternList [|dir|] patterns excludedDirs
            | Failure msg -> Failure msg
}   


let findMatchesInFile word (searchOptions: SearchOptions) (fileContent:string) = 
    let pattern = if searchOptions.HasFlag SearchOptions.WholeWords then @"\b" + word + @"\b" else word
    let regexOptions = RegexOptions.Compiled
    let ignoreCase = not (searchOptions.HasFlag SearchOptions.CaseSensitive)
    let regexOptions = if ignoreCase then regexOptions ||| RegexOptions.IgnoreCase else regexOptions
    let rx = Regex(pattern, regexOptions)
    rx.Matches(fileContent) |> Seq.map (fun m -> {Value = m.Value; Index = m.Index})   


let planFileMatches searchParams fileName = async {
    match fileName with
    | Success fileName ->
        let! fileContent = readFile fileName
        match fileContent with
        | Success fileContent -> 
            let matches = 
                match String.IsNullOrEmpty searchParams.Word with
                | false -> findMatchesInFile searchParams.Word searchParams.SearchOptions fileContent
                | true -> Seq.empty
            if Seq.isEmpty matches |> not then
                let fileContent = FileContent.create fileContent
                return FileSearchResult.create &fileContent {FileName=fileName; Matches=matches}
            else
                return NoMatches fileName
        | Failure msg -> return (Exn msg)
    | Failure msg -> return (Exn msg)
    // return {FileName=fileName; Matches=matches}
}


let planAllFilesMatches (searchParams: SearchParams) =
    let paths = Paths.value searchParams.Paths
    let patterns = Strings.value searchParams.Patterns
    let excludedDirs = Strings.value searchParams.ExcludedDirs
    getAllFilesByPatternList paths patterns excludedDirs
    |> Seq.map (fun fileName -> planFileMatches searchParams fileName |> Async.Catch) 
    |> Async.Parallel
    

let searchEvent = Event<Choice<FileSearchResultWithExn, exn>[]>()
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

