module QSearch.Gui

open Eto.Forms
open Eto.Drawing
open EtoUtils
open ResultTree

let btnSearch = new Button(Text="Search")
// let btnCancel = new Label(Text=string("Cancel"))
let edWord = new TextBox(Text="new")
let edPaths = new ComboBox(Text="C:/temp/temp6")
let edPatterns = new ComboBox(Text="*.*")
let edExcludedDirs = new ComboBox()
let cbCaseSensitive = new CheckBox(Text="Case sensitive")
let cbWholeWords = new CheckBox(Text="Whole words")
let cbRegExp = new CheckBox(Text="Regular expression")
let lblStatus = new Label()
let resultTree = new ResultTree()

let LabeledEdit text el = TableEl(
    Tbl[
        Spacing(Size(4, 4))
        Row[
             El(new Label(
                 Text=text,
                 VerticalAlignment=VerticalAlignment.Center,
                 TextAlignment=TextAlignment.Right,
                 Width=80
                 )
             )
             StretchedEl(el)
        ]
    ]
)

let table = Tbl[
    // Pad(Padding(2))
    Spacing(Size(4, 4))
    Row[
        TableEl(
            Tbl[
                Spacing(Size(2, 2))
                Row[
                     LabeledEdit "Search" edWord
                     El(btnSearch)
                ]
            ]
        )
    ]
    Row[ LabeledEdit "Paths" edPaths ]
    Row[ LabeledEdit "Patterns" edPatterns ]
    Row[ LabeledEdit "Excluded dirs" edExcludedDirs ]
    Row[ El(cbCaseSensitive) ]
    Row[ El(cbWholeWords) ]
    Row[ El(cbRegExp) ]
    Row[ El(new Label(Text = "Status")); El(lblStatus) ]
    StretchedRow[ StretchedEl(resultTree) ]
]

