#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli //"
#load "./.fake/build.fsx/intellisense.fsx"
// include Fake modules, see Fake modules section

open Fake.Core
open Fake.IO
open Fake.DotNet

let libdir = "src/malflib"
let clidir = "src/malfcli"

// *** Define Targets ***
Target.create "Clean" (fun _ ->
  Trace.log " --- Cleaning stuff --- "
  DotNet.exec id "clean" "" |> ignore
  Shell.cleanDirs [libdir + "/obj"; libdir + "/bin";
                       clidir + "/obj"; clidir + "/bin"]
)

Target.create "Build" (fun _ ->
   let options =  fun _ -> DotNet.Options.Create() |> fun x -> { x with Verbosity = Some(DotNet.Minimal)}
   Trace.log " --- Building the app --- "
   DotNet.exec options "build " "src/malfcli/malfcli.fsproj"
   |> ignore
)

Target.create "Deploy" (fun _ ->
  Trace.log " --- Deploying app --- "
)

Target.create "Test" (fun _ ->
    //Shell.Exec("bash", "dotests.sh", "..")
    //todo: just call tests from here instead of the shell script ?
    Shell.chdir ".."
    CreateProcess.fromRawCommand "bash" ["dotests.sh"]
    |> Proc.run
    |> ignore

)

open Fake.Core.TargetOperators

// *** Define Dependencies ***
"Clean"
  ==> "Build"
  ==> "Deploy"

"Build"
  ==> "Test"

// *** Start Build **
Target.runOrDefault "Build"