namespace GroupifyTest

open System
open System.IO
open System.Text.RegularExpressions
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

        let resultAST = 
            match parse input with
            | Some ast -> ast
            | None -> Num 0
        
        let expectedAST = Group (Operation("+%5"), Numbers [Num 0.0; Num 1.0; Num 2.0; Num 3.0; Num 4.0])

        let resultEvaluationStr, resultEvaluation = evaluator resultAST
        
        let expectedEvaluationStr = 
            """
            Numbers [Num 0.0; Num 1.0; Num 2.0; Num 3.0; Num 4.0] is a group under +%5 because:
            
            It is closed under +%5.

            The identity element is 0.
            
            Every element has an inverse: [(2, 3); (1, 4); (0, 0)].
            
            +%5 is associative.
            """
        
        let expectedEvaluation = true

        Assert.AreEqual(expectedAST, resultAST)
        Assert.AreEqual(Regex.Replace(expectedEvaluationStr, @"\s+", ""), Regex.Replace(resultEvaluationStr, @"\s+", ""))
        Assert.AreEqual(expectedEvaluation, resultEvaluation)

    [<TestMethod>]
    member this.TestTwo () =

        let file = "/Users/jonathoncarl/f22/cs334/cs334_project-jonathonacarl-sarahf64/code/backend/GroupifyTest/examples/example-2.groupify"
        let input = File.ReadAllText file

        let resultAST = 
            match parse input with
            | Some ast -> ast
            | None -> Num 0
        
        let expectedAST = Group (Operation("+"), Integers("Z"))

        let resultEvaluationStr, resultEvaluation = evaluator resultAST

        let expectedEvaluationStr =
            """
            "Z" is a group under + because:

            It is closed under +.
            
            The identity element is 0.
            
            Every element has an inverse.
            
            + is associative.
            
            """

        let expectedEvaluation = true

        Assert.AreEqual(expectedAST, resultAST)
        Assert.AreEqual(Regex.Replace(expectedEvaluationStr, @"\s+", ""), Regex.Replace(resultEvaluationStr, @"\s+", ""))
        Assert.AreEqual(expectedEvaluation, resultEvaluation)

    [<TestMethod>]
    member this.TestThree () =

        let file = "/Users/jonathoncarl/f22/cs334/cs334_project-jonathonacarl-sarahf64/code/backend/GroupifyTest/examples/example-3.groupify"
        let input = File.ReadAllText file

        let resultAST = 
            match parse input with
            | Some ast -> ast
            | None -> Num 0
        
        let expectedAST = Group (Operation("*"), Numbers [Num -1.0; Num 1.0])

        let resultEvaluationStr, resultEvaluation = evaluator resultAST

        let expectedEvaluationStr = 
            """
            Numbers [Num -1.0; Num 1.0] is a group under * because:

            It is closed under *.

            The identity element is 1.

            Every element has an inverse: [(1, 1); (-1, -1)].

            * is associative.

            """
        
        let expectedEvaluation = true

        Assert.AreEqual(expectedAST, resultAST)
        Assert.AreEqual(Regex.Replace(expectedEvaluationStr, @"\s+", ""), Regex.Replace(resultEvaluationStr, @"\s+", ""))
        Assert.AreEqual(expectedEvaluation, resultEvaluation)

    [<TestMethod>]
    member this.TestFour () =

        let file = "/Users/jonathoncarl/f22/cs334/cs334_project-jonathonacarl-sarahf64/code/backend/GroupifyTest/examples/example-4.groupify"
        let input = File.ReadAllText file

        let resultAST = 
            match parse input with
            | Some ast -> ast
            | None -> Num 0
        
        let expectedAST = Group (Operation("/"), Numbers [Num 1.0; Num 2.0; Num 3.0])

        let resultEvaluationStr, resultEvaluation = evaluator resultAST

        let expectedEvaluationStr = 
            """
            Numbers [Num 1.0; Num 2.0; Num 3.0] is not a group under / because:
            
            It is not closed. Notice that 1,2 are in Numbers [Num 1.0; Num 2.0; Num 3.0], but 1 / 2 = 0.5 is not in Numbers [Num 1.0; Num 2.0; Num 3.0].
            
            It contains no identity element.
            
            1 is an element with no inverse.
            
            It is not associative.
            
            """

        let expectedEvaluation = false

        Assert.AreEqual(expectedAST, resultAST)
        Assert.AreEqual(Regex.Replace(expectedEvaluationStr, @"\s+", ""), Regex.Replace(resultEvaluationStr, @"\s+", ""))
        Assert.AreEqual(expectedEvaluation, resultEvaluation)

    
    [<TestMethod>]
    member this.TestFive () =

        let file = "/Users/jonathoncarl/f22/cs334/cs334_project-jonathonacarl-sarahf64/code/backend/GroupifyTest/examples/example-5.groupify"
        let input = File.ReadAllText file

        let resultAST = 
            match parse input with
            | Some ast -> ast
            | None -> Num 0
        
        let expectedAST = Group (Operation("-"), Numbers [Num 1.0; Num 2.0; Num 3.0; Num 4.0])

        let resultEvaluationStr, resultEvaluation = evaluator resultAST

        let expectedEvaluationStr = 
            """
            Numbers [Num 1.0; Num 2.0; Num 3.0; Num 4.0] is not a group under - because:
            
            It is not closed. Notice that 1,1 are in Numbers [Num 1.0; Num 2.0; Num 3.0; Num 4.0], but 1 - 1 = 0 is not in Numbers [Num 1.0; Num 2.0; Num 3.0; Num 4.0].
            
            It contains no identity element.
            
            4 is an element with no inverse.
            
            It is not associative.
            
            """

        let expectedEvaluation = false

        Assert.AreEqual(expectedAST, resultAST)
        Assert.AreEqual(Regex.Replace(expectedEvaluationStr, @"\s+", ""), Regex.Replace(resultEvaluationStr, @"\s+", ""))
        Assert.AreEqual(expectedEvaluation, resultEvaluation)

    [<TestMethod>]
    member this.TestSix () =

        let file = "/Users/jonathoncarl/f22/cs334/cs334_project-jonathonacarl-sarahf64/code/backend/GroupifyTest/examples/example-6.groupify"
        let input = File.ReadAllText file

        let resultAST = 
            match parse input with
            | Some ast -> ast
            | None -> Num 0
        
        let expectedAST = Group (Operation("+"), Reals("R*"))

        let resultEvaluationStr, resultEvaluation = evaluator resultAST

        let expectedEvaluationStr = 
            """
            "R*" is not a group under + because it is not closed.

            """

        let expectedEvaluation = false

        Assert.AreEqual(expectedAST, resultAST)
        Assert.AreEqual(Regex.Replace(expectedEvaluationStr, @"\s+", ""), Regex.Replace(resultEvaluationStr, @"\s+", ""))
        Assert.AreEqual(expectedEvaluation, resultEvaluation)