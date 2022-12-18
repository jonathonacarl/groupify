open Parser
open Evaluator
open System.IO

[<EntryPoint>]
let main argv = 

    // toggle verbose on with --v (--verbose) flag
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
    
    // parse from file or directly from CLI
    let i =
        try 
            File.ReadAllText argv[0]
        with
        | :? FileNotFoundException -> 
            printfn $"file not found: parsing again...\n"
            "file not found"
    
    let input = if i.Equals("file not found") then argv[0] else i

    let ast = parse input

    match ast with
    | Some ast ->
        let str, res = evaluator ast
        if verbose then printfn "%A" str else printfn "%A" res
        0
    | None -> 
        printfn "Invalid program\n usage: dotnet run \"<set> <operation>\" --verbose (optional)"
        1