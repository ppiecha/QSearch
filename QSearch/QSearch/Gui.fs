﻿module QSearch.Gui

open Eto.Forms
open EtoUtils

let button = new Button(Text="+1")
let display = new Label(Text=string(counter))
let table = Tbl[ Row[ El(display) ]; Row[ El(button) ]]

