open Parser
open Evaluator
open System.IO

[<EntryPoint>]
let main argv = 

    let verbose = 
        match argv.Length with
        | 1 -> 
            false
        | 2 ->
            if argv[1].Equals("--v") || argv[1].Equals("--verbose") then 
                true 
            else
                printfn "usage: dotnet run \"<set> <operation>\" --v (or --verbose)"
                printfn "ex: \"{0,1,2,3,4} +%%5\" "
                exit 1
        | _ ->
            printfn "usage: dotnet run \"<set> <operation>\" --verbose (optional)"
            printfn "ex: \"{0,1,2,3,4} +%%5\" "
            exit 1

    // let input = argv[0]

    let file = argv[0]
    let input = File.ReadAllText file

    let ast = parse input

    match ast with
    | Some ast ->
        let str, res = evaluator ast
        if verbose then printfn "%A" str else printfn "%A" res
        0
    | None -> 
        printfn "Invalid program."
        1