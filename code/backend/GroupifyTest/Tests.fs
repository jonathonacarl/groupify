namespace GroupifyTest

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser
open Evaluator


[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestOne () =

        let file = "/Users/jonathoncarl/f22/cs334/cs334_project-jonathonacarl-sarahf64/code/backend/GroupifyTest/examples/example-1.groupify"

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

        Assert.AreEqual(resultParse, expectedParse)
        Assert.AreEqual(resultAST, expectedAST)
        Assert.AreEqual(resultEvaluationStr, expectedEvaluationStr)
        Assert.AreEqual(resultEvaluation, expectedEvaluation)

    [<TestMethod>]
    member this.TestTwo () =

        let file = "/Users/jonathoncarl/f22/cs334/cs334_project-jonathonacarl-sarahf64/code/backend/GroupifyTest/examples/example-2.groupify"
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

        Assert.AreEqual(resultParse, expectedParse)
        Assert.AreEqual(resultAST, expectedAST)
        Assert.AreEqual(resultEvaluationStr, expectedEvaluationStr)
        Assert.AreEqual(resultEvaluation, expectedEvaluation)

    [<TestMethod>]
    member this.TestThree () =

        let file = "/Users/jonathoncarl/f22/cs334/cs334_project-jonathonacarl-sarahf64/code/backend/GroupifyTest/examples/example-3.groupify"
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

        Assert.AreEqual(resultParse, expectedParse)
        Assert.AreEqual(resultAST, expectedAST)
        Assert.AreEqual(resultEvaluationStr, expectedEvaluationStr)
        Assert.AreEqual(resultEvaluation, expectedEvaluation)

    [<TestMethod>]
    member this.TestFour () =

        let file = "/Users/jonathoncarl/f22/cs334/cs334_project-jonathonacarl-sarahf64/code/backend/GroupifyTest/examples/example-4.groupify"
        let input = File.ReadAllText file

        let resultParse = parse input

        let expectedParse = "Some(Group (Operation(\"/\"), Numbers [Num 1.0; Num 2.0; Num 3.0))"

        let resultAST = 
            match resultParse with
            | Some ast -> ast
            | None -> Num 0
        
        let expectedAST = "Group (Operation(\"/\"), Numbers [Num 1.0; Num 2.0; Num 3.0)"

        let resultEvaluationStr, resultEvaluation = evaluator resultAST

        let expectedEvaluationStr = "Numbers [Num 1.0; Num 2.0; Num 3.0] is a not group under / because:\n\nIt is not closed. Notice that 1,2 are in Numbers [Num 1.0; Num 2.0; Num 3.0], but 1 / 2 = 0.5 is not in Numbers [Num 1.0; Num 2.0; Num 3.0].\n\nIt contains no identity element.\n\n1 is an element with no inverse.\n\nIt is not associative."

        let expectedEvaluation = false

        Assert.AreEqual(resultParse, expectedParse)
        Assert.AreEqual(resultAST, expectedAST)
        Assert.AreEqual(resultEvaluationStr, expectedEvaluationStr)
        Assert.AreEqual(resultEvaluation, expectedEvaluation)

    
    [<TestMethod>]
    member this.TestFive () =

        let file = "/Users/jonathoncarl/f22/cs334/cs334_project-jonathonacarl-sarahf64/code/backend/GroupifyTest/examples/example-5.groupify"
        let input = File.ReadAllText file

        let resultParse = parse input

        let expectedParse = "Some(Group (Operation(\"-\"), Numbers [Num 1.0; Num 2.0; Num 3.0; Num 4.0))"

        let resultAST = 
            match resultParse with
            | Some ast -> ast
            | None -> Num 0
        
        let expectedAST = "Group (Operation(\"-\"), Numbers [Num 1.0; Num 2.0; Num 3.0; Num 4.0)"

        let resultEvaluationStr, resultEvaluation = evaluator resultAST

        let expectedEvaluationStr = "Numbers [Num 1.0; Num 2.0; Num 3.0; Num 4.0] is a not group under - because:\n\nIt is not closed. Notice that 1,1 are in Numbers [Num 1.0; Num 2.0; Num 3.0; Num 4.0], but 1 - 1 = 0 is not in Numbers [Num 1.0; Num 2.0; Num 3.0; Num 4.0].\n\nIt contains no identity element.\n\n4 is an element with no inverse.\n\nIt is not associative."

        let expectedEvaluation = false

        Assert.AreEqual(resultParse, expectedParse)
        Assert.AreEqual(resultAST, expectedAST)
        Assert.AreEqual(resultEvaluationStr, expectedEvaluationStr)
        Assert.AreEqual(resultEvaluation, expectedEvaluation)

    [<TestMethod>]
    member this.TestSix () =

        let file = "/Users/jonathoncarl/f22/cs334/cs334_project-jonathonacarl-sarahf64/code/backend/GroupifyTest/examples/example-6.groupify"
        let input = File.ReadAllText file

        let resultParse = parse input

        let expectedParse = "Some(Group (Operation(\"+\"), \"Z\")"

        let resultAST = 
            match resultParse with
            | Some ast -> ast
            | None -> Num 0
        
        let expectedAST = "Group (Operation(\"+\"), \"Z\")"

        let resultEvaluationStr, resultEvaluation = evaluator resultAST

        let expectedEvaluationStr = "\"Z\" is a group under + because:\n\nIt is closed under +.\n\n The identity element is 0.\n\nEvery element has in inverse.\n\n+ is associative."

        let expectedEvaluation = false

        Assert.AreEqual(resultParse, expectedParse)
        Assert.AreEqual(resultAST, expectedAST)
        Assert.AreEqual(resultEvaluationStr, expectedEvaluationStr)
        Assert.AreEqual(resultEvaluation, expectedEvaluation)