module QSearch.Search

open System.IO
open System.Text.RegularExpressions

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

type Match = {Path: string; Value: string; Index: int}

let findMatchesInFile text ignoreCase wholeWords (path:string) = async {
    let! content = readFile path
    let pattern = if wholeWords then text else @"\b" + text + @"\b"
    let options = RegexOptions.Compiled
    let options = if ignoreCase then options + RegexOptions.IgnoreCase else options
    let rx = Regex(pattern, options)
    return rx.Matches(content) |> Seq.map (fun m -> {Path = path; Value = m.Value; Index = m.Index})   
}

