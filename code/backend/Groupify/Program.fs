open Parser
open Evaluator
open System.IO

[<EntryPoint>]
let main argv = 

    if argv.Length <> 1 then
        printfn "usage: dotnet run \"<set> <operation>\" "
        printfn "ex: \"{0,1,2,3,4,} +%%5\" "
        printfn "Note: please include comma after last element in set."
        exit 1

    let file = argv[0]
    let input = File.ReadAllText file

    let ast = parse input

    match ast with
    | Some ast ->
        let str, res = evaluator ast
        printfn "%A" str
        printfn "%A" ast
        0
    | None -> 
        printfn "Invalid program."
        1
