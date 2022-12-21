namespace QSearch.Mac
module Program =

    open System
    open QSearch

    [<EntryPoint>]
    [<STAThread>]
    let Main(args) = 
        let app = new Eto.Forms.Application(Eto.Platforms.Mac64)
        app.Run(new MainForm())
        0