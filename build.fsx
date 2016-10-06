// include Fake lib
#r "packages/FAKE/tools/FakeLib.dll"
open Fake

// Properties
let buildDir = "./build/"
let debugDir = "./debug/"


// Targets
Target "Clean" (fun _ ->
 CleanDir buildDir
)

Target "BuildTest" (fun _ ->
!! "/**/*.fsproj" |> MSBuildDebug debugDir  "Build" |> Log "TestBuild-Output: "
)


// Dependencies
"Clean"
  ==> "Default"

// start build
RunTargetOrDefault "Default"
