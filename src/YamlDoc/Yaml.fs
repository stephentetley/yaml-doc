// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace YamlDoc

[<AutoOpen>]
module Yaml =

    /// This is just a dummy to get the feel of Yaml.
    /// A real implementation will probably uses a skeleton syntax
    /// like Markdown-doc.

    open SLFormat.Pretty

    type Yaml = Doc

    let render (source:Yaml) : string = 
        SLFormat.Pretty.render 80 source

    let comment (body:string) : Yaml =
        character '#' ^+^ text body 

    let docStart : Yaml = text "---"

    let docEnd : Yaml = text "..."

    let vlist (items:Yaml list) : Yaml = 
        items |> List.map (fun doc -> character '-' ^+^ doc) |> vcat 

    let keyvalue (key:Yaml) (value:Yaml) : Yaml  = 
        key ^^ character ':' ^+^ value

    let typename (name:string) : Yaml = 
        text "!!" ^^ text name

    let yamlInt (value:int) : Yaml = 
        intDoc value

    let yamlDouble (value:double) : Yaml = 
        doubleDoc value
