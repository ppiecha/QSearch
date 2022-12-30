namespace QSearch

open System.Threading
open Eto.Forms
open Eto.Drawing
open EtoUtils
open QSearch.Types

type MainForm () as self =
    inherit Form()
    let ctx = SynchronizationContext.Current
    let layout = makeLayout Gui.table
    let mutable cts = new CancellationTokenSource()
    do
        base.Title <- "Quick Search"
        base.MinimumSize <- new Size(200, 200)
        base.Padding <- new Padding(4)

        base.Content <- layout
        
        Search.searchFinished.Add(fun e ->
            Gui.resultTree.populateTree e
            Gui.lblStatus.Text <- $"Completed %A{e.Length}"
            )
        Gui.btnSearch.Click.Add(fun e -> self.runSearch ())
        
    member self.runSearch () =
        match self.prepareSearchParams () with
        | Success searchParams ->
              cts.Dispose()
              cts <- Search.searcher.PostAndReply (fun rc -> searchParams, rc, ctx)
              printfn $"Search started %A{searchParams}"
        | Failure errors -> MessageBox.Show(errors |> String.concat "\n", MessageBoxType.Error) |> ignore
    
    member self.prepareSearchParams () =
        let errors = [||]
        let searchParams = {
            Paths=Paths.init
            Patterns=Strings.init
            Word=Gui.edWord.Text
            ExcludedDirs=Strings.init
            SearchOptions=SearchOptions.None
        }
        let searchParams, errors = 
            match Gui.edPaths.Text |> Paths.createFromSSV with
            | Success paths -> {searchParams with Paths=paths}, errors
            | Failure pathsErrors -> searchParams, (Array.append errors pathsErrors) 
        
        let searchParams, errors = 
            match Gui.edPatterns.Text |> Strings.createFromSSV with
            | Success patterns -> {searchParams with Patterns=patterns}, errors
            | Failure f -> searchParams, (Array.append errors f)
            
        let searchParams, errors = 
            match Gui.edExcludedDirs.Text |> Strings.createFromSSV with
            | Success excludedDirs -> {searchParams with ExcludedDirs=excludedDirs}, errors
            | Failure f -> searchParams, (Array.append errors f)             
               
        let searchOptions = SearchOptions.None
        let searchOptions =
            searchOptions |||
            match isNullableBoolTrue Gui.cbCaseSensitive.Checked with
            | true -> SearchOptions.CaseSensitive
            | false -> searchOptions
        let searchOptions =
            searchOptions |||
            match isNullableBoolTrue Gui.cbWholeWords.Checked with
            | true -> SearchOptions.WholeWords
            | false -> searchOptions
        let searchOptions =
            searchOptions |||
            match isNullableBoolTrue Gui.cbRegExp.Checked with
            | true -> SearchOptions.RegExp
            | false -> searchOptions
            
        let searchParams = {searchParams with SearchOptions=searchOptions}        
            
        if Array.isEmpty errors then Success searchParams else Failure errors
            
        
        
    
        
    
        
               

            
    
