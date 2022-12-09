namespace GroupifyTest

open System.IO
open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser
open Evaluator


[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestOne () =

        let file = "./examples/example-1.groupify"
        let input = File.ReadAllText file

        let resultParse = parse input

        let expectedParse = "Some(Group (Operation(\"+%5\"), Numbers [Num 0.0; Num 1.0; Num 2.0; Num 3.0; Num 4.0]))"

        let resultAST = 
            match resultParse with
            | Some ast -> ast
            | None -> Num 0
        
        let expectedAST = "Group (Operation(\"+%5\"), Numbers [Num 0.0; Num 1.0; Num 2.0; Num 3.0; Num 4.0])"

        let resultEvaluationStr, resultEvaluation = evaluator resultAST
        
        let expectedEvaluationStr = "Numbers [Num 0.0; Num 1.0; Num 2.0; Num 3.0; Num 4.0] is a group under +%5 because:\n\nIt is closed under +%5\n\nThe identity element is 0\n\nEvery element has an inverse: [(2, 3); (1, 4); (0, 0)] \n\n+%5 is associative."
        
        let expectedEvaluation = true

        Assert.AreEqual(expectedParse, resultParse)
        Assert.AreEqual(expectedAST, resultAST)
        Assert.AreEqual(expectedEvaluationStr, resultEvaluationStr)
        Assert.AreEqual(expectedEvaluation, resultEvaluation)

    [<TestMethod>]
    member this.TestTwo () =

        let file = "./examples/example-2.groupify"
        let input = File.ReadAllText file

        let resultParse = parse input

        let expectedParse = "Some(Group (Operation(\"+%4\"), Numbers [Num 0.0; Num 1.0; Num 2.0; Num 3.0]))"

        let resultAST = 
            match resultParse with
            | Some ast -> ast
            | None -> Num 0
        
        let expectedAST = "Group (Operation(\"+%4\"), Numbers [Num 0.0; Num 1.0; Num 2.0; Num 3.0])"

        let resultEvaluationStr, resultEvaluation = evaluator resultAST

        let expectedEvaluationStr = "Numbers [Num 0.0; Num 1.0; Num 2.0; Num 3.0] is a group under +%4 because:\n\nIt is closed under +%4\n\nThe identity element is 0\n\nEvery element has an inverse: [(2, 2); (1, 3); (0, 0)] \n\n+%4 is associative."

        let expectedEvaluation = true

        Assert.AreEqual(expectedParse, resultParse)
        Assert.AreEqual(expectedAST, resultAST)
        Assert.AreEqual(expectedEvaluationStr, resultEvaluationStr)
        Assert.AreEqual(expectedEvaluation, resultEvaluation)

    [<TestMethod>]
    member this.TestThree () =

        let file = "./examples/example-3.groupify"
        let input = File.ReadAllText file

        let resultParse = parse input

        let expectedParse = "Some(Group (Operation(\"+%3\"), Numbers [Num -1.0; Num 0.0; Num 1.0))"

        let resultAST = 
            match resultParse with
            | Some ast -> ast
            | None -> Num 0
        
        let expectedAST = "Group (Operation(\"+%3\"), Numbers [Num -1.0; Num 0.0; Num 1.0)"

        let resultEvaluationStr, resultEvaluation = evaluator resultAST

        let expectedEvaluationStr = "Numbers [Num -1.0; Num 0.0; Num 1.0] is a not group under +%3 because:\n\nIt is not closed. Notice that -1,-1 are in Numbers [Num -1.0; Num 0.0; Num 1.0], but (-1 + -1) % 3 = -2 is not in Numbers [Num -1.0; Num 0.0; Num 1.0].\n"

        let expectedEvaluation = false

        Assert.AreEqual(expectedParse, resultParse)
        Assert.AreEqual(expectedAST, resultAST)
        Assert.AreEqual(expectedEvaluationStr, resultEvaluationStr)
        Assert.AreEqual(expectedEvaluation, resultEvaluation)