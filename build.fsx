// include Fake lib
#r "packages/FAKE/tools/FakeLib.dll"
open Fake

// Properties
let buildModeRelease = getBuildParamOrDefault "buildMode" "Release"
let buildModeDebug = getBuildParamOrDefault "buildMode" "Debug"

let webDir = "/web"
let currentPath = FileSystemHelper.currentDirectory
let sitePath = currentPath + "/Web.Site"
let releaseDir = "/bin/Release"
let debugDir = "/bin/Debug"
let webPath = sitePath + webDir
let cleanPath = sitePath
let jsonDataPath = sitePath + "/jsonData.json"
let setParams buildMode defaults =
        { defaults with
            Verbosity = Some(Quiet)
            Targets = ["Build"]
            Properties =
                [
                    "Optimize", "True"
                    "DebugSymbols", "True"
                    "Configuration", buildMode
                ]
         }

let commonBuildActions buildDir =
    let targetDir = (sitePath + buildDir)
    CopyDir (targetDir + webDir)  webPath (fun _ -> true) |> ignore
    CopyFile targetDir jsonDataPath |> ignore

Target "Release" (fun _ -> 
    build (buildModeRelease |> setParams) "./WebServer.sln" |> DoNothing
    commonBuildActions releaseDir |> ignore
)

Target "Default" (fun _ -> 
    build (buildModeDebug |> setParams) "./WebServer.sln" |> DoNothing
    commonBuildActions debugDir |> ignore
)

Target "Clean" (fun _ ->
    CleanDir (sitePath + debugDir)
    CleanDir (sitePath + releaseDir)
)

RunTargetOrDefault "Default"