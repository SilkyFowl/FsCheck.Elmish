#if FAKE
#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.DotNet.Testing.XUnit2
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"
#endif
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

// Properties
let buildDir = "./build/"
let testDir = "./tests/*.Tests/bin/Release/net6.0/"

Target.initEnvironment ()

// Target
Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "tests/**/bin"
    ++ "tests/**/obj"
    |> Shell.cleanDirs)

Target.create "Build" (fun _ ->
    !! "src/**/*.*proj" ++ "tests/**/*.*proj"
    |> Seq.iter (DotNet.build id))

Target.create "Test" (fun _ ->
    !! "tests/**/*.*proj"
    |> Seq.iter (DotNet.test id))

Target.create "All" ignore

"Clean" ==> "Build" ==> "All"

Target.runOrDefault "All"
