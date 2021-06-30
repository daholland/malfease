#r "nuget:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli //"
#load "./.fake/build.fsx/intellisense.fsx"
// include Fake modules, see Fake modules section

open Fake.Core
open Fake.IO
open Fake.DotNet

// *** Define Targets ***
Target.create "Clean" (fun _ ->
  Trace.log " --- Cleaning stuff --- "
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

Target.create "test" (fun _ ->
    //Shell.Exec("bash", "dotests.sh", "..")
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

"Clean"
  ==> "test"

// *** Start Build **
Target.runOrDefault "test"