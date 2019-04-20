// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190322\lib\netstandard2.0"
#r "SLFormat.dll"


#load "..\src\YamlDoc\Internal\Common.fs"
#load "..\src\YamlDoc\Yaml.fs"
open YamlDoc

let demo01 () = 
    comment "hello world" |> render