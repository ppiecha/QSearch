module QSearch.ResultTree
open Eto.Forms
open Types
open System


type ResultTree () as self =
    inherit TreeGridView()
    
    let mutable treeResultsCollection = TreeGridItemCollection()
    
    do
        self.ShowHeader <- true
        self.Columns.Add(new GridColumn(DataCell = new TextBoxCell(0), AutoSize=true, HeaderText="ppp"))
        self.DataStore <- treeResultsCollection
    
    member self.create_item (text: string) (children): ITreeGridItem =
        TreeGridItem(values = [|box(text)|], children=children) :> ITreeGridItem
        
        
    member self.fileItem (fileSearchResultWithExn: FileSearchResultWithExn) = 
        match fileSearchResultWithExn with
        | FileSearchResult {FileResult=fileResult; FileLineResults=fileLineResults} ->           
            let children = fileLineResults |> Array.map (fun flr -> self.create_item flr.LineText [])
            // printfn $"{fileResult.FileName}"
            let text = $"{fileResult.FileName} ({fileResult.NumberOfMatches} matches)"
            // let text = text |> String
            printfn $"{fileResult.FileName} ({fileResult.NumberOfMatches} matches)"
            self.create_item text children
        | Exn str -> self.create_item str []
        
    member self.populateTree (fileSearchResults: Choice<FileSearchResult, exn>[]) =
        treeResultsCollection.Clear()   
        fileSearchResults
        |> Array.map self.resultsToGridCollection
        |> Array.map self.fileItem
        //|> TreeGridItemCollection
        |> treeResultsCollection.AddRange
        printfn $"count {treeResultsCollection.Count}"
        self.ReloadData()
        printfn $"count {treeResultsCollection.Count}"
        
    member self.resultsToGridCollection = function
        | Choice1Of2 fsr -> FileSearchResult fsr
        | Choice2Of2 exn -> Exn exn.Message
        
    
