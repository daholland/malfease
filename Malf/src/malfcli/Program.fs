open System
open Malf


// let inputloop =
//     let mutable continueRead = true
//     while continueRead do
//         printf "user> "
//         let input = Console.ReadLine()
//         match input with
//         | null -> continueRead <- false
//         | inputString -> Malf.REP inputString
//                          |> printfn "%s"


[<EntryPoint>]
let main argv =
    // for a in argv do printfn "argv: %s" a
    // printfn "arg len: %d" argv.Length

    let mutable continueRead = true

    if argv.Length > 0
         then (Malf.REP argv.[0]) |> printfn "%s"
         else while continueRead do
                printf "user> "
                let input = Console.ReadLine()
                match input with
                 | null -> continueRead <- false
                 | inputString -> Malf.REP inputString
                                    |> printfn "%s"
    0 // return an integer exit code
      //
      //
      //
      //
      // (projectile-run-project "cd ../../..; ./runmaltests 2")
