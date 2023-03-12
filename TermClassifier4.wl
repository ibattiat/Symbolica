(* ::Package:: *)

(* Symbolica \[Copyright] 2023 by Kyle Pietrzyk is licensed under Attribution-NonCommercial-ShareAlike 4.0 International *)


(****** TermClassifier.wl -- Create a vector of the term and their scaling coefficient.
*************)

(* 

TermClassifier[term, spacial independent variables, spacial independent variable scales,
time independent variable, time independent variable scales] 

Given a term, the dependent variables, a list of spacial independent variables, time independent variables, and their
scales, this code scales the term and records the term with its dimensionless coefficient.

*)


(* Created December 6 2021
Modified June 14 2020: 
                       *)



BeginPackage["Disruptioneering`TermClassifier`"];

TermClassifier::usage = 
"TermClassifier[term, dependent variables, spacial independent variables, spacial independent
variable scales, time independent variable, time independent variable scales]  Given a term,
the dependent variables, a list of spacial independent variables, time independent variables,
and their scales, this code scales the term and records the term with its dimensionless
coefficient.";



Begin["`Disruptioneering`"];

(*avgdenoters should be {{Subscript[\[LeftAngleBracket]spaceholder\[RightAngleBracket], sub], sub, spaceholder},
{{{\[CapitalOmega],\[CapitalGamma]},{\[Xi],\[Eta]},{{\[CapitalOmega]a,\[CapitalOmega]b},{\[CapitalGamma]a,\[CapitalGamma]b}}},{{\[CapitalOmega]a,\[CapitalGamma]a},{\[Xi], \[Eta]},{{},{\[CapitalGamma]ai,\[CapitalGamma]ao}}},
{{\[CapitalOmega]b,\[CapitalGamma]b},{\[Xi], \[Eta]},{{},{\[CapitalGamma]bi,\[CapitalGamma]bo}}}{{\[CapitalDelta]\[Tau]PE1,\[Delta]\[Tau]PE1}, {\[Tau]Pe1},{{},{}}}}}*)
(*graddenoters should be {Grad[spaceholder, sub], sub, spaceholder}*)
(*divdenoters should be {Div[spaceholder, sub], sub, spaceholder}*)

TermClassifier[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, \
closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, \
dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, option_] := 
    procedure[term, variables, variablescales, variableKSAS, scalars, vectors, tensors, \
      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
      dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
      divform, gradform, normalvecform, Uform, slowIV, fastIV, option];


procedure[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, \
closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, \
dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, option_] :=
    Block[{solution, globalNoUnidentifiedSymbols},
        
        globalNoUnidentifiedSymbols = option;
        
        solution = main[Expand[term], variables, variablescales, variableKSAS, scalars, vectors, \
          tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
          dotdenoters, tensorproductdenoters, doubledotdenoters, \
          pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
          fastIV, {}];
        
        solution = Expand[solution];
        
        Return[solution];
    ];


main[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, \
gradform_, normalvecform_, Uform_, slowIV_, fastIV_, option_] :=
    Block[{solution},
        
        If[Head[term] == List,
            solution = caseListmain[term, variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, \
              graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
              doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              option];
            ,
            solution = caseNonListmain[term, variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, \
              graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
              doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              option];
            ,
            solution = caseNonListmain[term, variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, \
              graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
              doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              option];
        ];
        
        Return[solution];
    ];


caseListmain[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, option_] :=
    Block[{i, solution = ConstantArray[0, Length[term]]},
        
        For[i = 1, i <= Length[term], i++,
            solution[[i]] = main[term[[i]], variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, \
              graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
              doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              option];
        ];
        
        Return[solution];
    ];


caseNonListmain[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, option_] :=
    Block[{i, solution, solutionvec},
                
        If[Head[term] == Plus,
            solutionvec = ConstantArray[0, Length[term]];
            For[i = 1, i <= Length[term], i++,
                solutionvec[[i]] = casePlusSubstituter[term[[i]], variables, variablescales, variableKSAS, scalars, \
                  vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, option];
            ];
            
            solution = solutionvec;
            ,
            solution = casePlusSubstituter[term, variables, variablescales, variableKSAS, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, option];
            If[Head[solution] == List && Head[Head[solution]] == List,
                ,
                solution = {solution};
                ,
                solution = {solution};
            ];
            ,
            solution = casePlusSubstituter[term, variables, variablescales, variableKSAS, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, option];
            If[Head[solution] == List && Head[Head[solution]] == List,
                ,
                solution = {solution};
                ,
                solution = {solution};
            ];
        ];
        
        Return[solution];
    ];


casePlusSubstituter[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, \
gradform_, normalvecform_, Uform_, slowIV_, fastIV_, option_] :=
    Block[{solution, levelcountdummy = 0, i},
        
        solution = decider[term, variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, \
          graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
          normalvecform, Uform, slowIV, fastIV, levelcountdummy, {}];
        
        If[solution == 1,
            If[globalNoUnidentifiedSymbols == "No Unidentified Symbols",
                solution = 1;
                ,
                solution = 0;
                ,
                solution = 0;
            ];
        ];
        
        Return[solution];
    ];


decider[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, \
normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, levelcount = levelcountdummy + 1, chosen = 0, dependencies, i, \
    flagsubscript = 0},
        
        printouttoggle = 0;
        
        If[printouttoggle == 1,
            Print[term];                
        ];
        If[levelcount == 50,
            
            Print["decider: Reached levelcount limit. Backing out."];
            
            ,
            
            If[Head[term] == List && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified List"];                
                ];
                chosen = 1;
                dimlesscoef = caseList[term, variables, variablescales, variableKSAS, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Rational && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Rational"];                
                ];
                chosen = 1;
                dimlesscoef = caseRational[term, variables, variablescales, variableKSAS, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            If[Head[term] == Integer && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Integer"];                
                ];
                chosen = 1;
                dimlesscoef = caseInteger[term, variables, variablescales, variableKSAS, scalars, vectors, \
                  tensors,closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            If[Head[term] == Symbol && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Symbol"];                
                ];
                chosen = 1;
                dimlesscoef = caseSymbol[term, variables, variablescales, variableKSAS, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            If[Head[term] == Head[pietrzykflipdenoters[[1]]] && chosen == 0,
                If[term[[2]] == pietrzykflipdenoters[[2]],
                    If[printouttoggle == 1,
                        Print["getterminfo: Identified Pietrzyk Flip"];                
                    ];
                    chosen = 1;
                    dimlesscoef = casePietrzykFlip[term, variables, variablescales, variableKSAS, scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                      gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                ];
            ];
            If[Head[term] == Power && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Power"];                
                ];
                chosen = 1;
                dimlesscoef = casePower[term, variables, variablescales, variableKSAS, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[derivativedenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Derivative"];                
                ];
                chosen = 1;
                dimlesscoef = caseDerivative[term, variables, variablescales, variableKSAS, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            If[Head[term] == Times && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Times"];                
                ];
                chosen = 1;
                dimlesscoef = caseTimes[term, variables, variablescales, variableKSAS, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[dotdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Dot"];                
                ];
                chosen = 1;
                dimlesscoef = caseDot[term, variables, variablescales, variableKSAS, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[tensorproductdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified TensorProduct"];                
                ];
                chosen = 1;
                dimlesscoef = caseTensorProduct[term, variables, variablescales, variableKSAS, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            If[Head[term] == Head[doubledotdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified DoubleDot"];                
                ];
                chosen = 1;
                dimlesscoef = caseDoubleDot[term, variables, variablescales, variableKSAS, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Plus && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Plus"];                
                ];
                chosen = 1;
                dimlesscoef = casePlus[term, variables, variablescales, variableKSAS, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[graddenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Gradient"];
                ];
                chosen = 1;
                dimlesscoef = caseGradient[term, variables, variablescales, variableKSAS, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            If[Head[term] == Head[divdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Divergence"];
                ];
                chosen = 1;
                dimlesscoef = caseDivergence[term, variables, variablescales, variableKSAS, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            If[Head[term] == Head[avgdenoters[[1, 1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Average"];
                ];
                chosen = 1;
                dimlesscoef = caseAverage[term, variables, variablescales, variableKSAS, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            
            If[chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified None"];                
                ];
                If[Head[Head[term]] == Symbol,
                    dependencies = ConstantArray[0, Length[term]];
                    For[i = 1, i <= Length[term], i++,
                        dependencies[[i]] = term[[i]];
                    ];
                ];
                dimlesscoef = decider[Head[term], variables, variablescales, variableKSAS, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  dependencies];
            ];
            
            ,
            
            If[printouttoggle == 1,
                Print["Idk"];                
            ];
            
        ];
        
        If[printouttoggle == 1,
            Print[dimlesscoef];
        ];
        
        Return[dimlesscoef];
        
    ];


(*Ready*)
caseList[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{},
        Print["CRITICAL ERROR: caseList: Not written yet (but there shouldnt be any lists). Term is: ", term];
        Return[Null];
    ];


(*Ready*)
caseRational[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = 1},
        Return[dimlesscoef];
    ];


(*Ready*)
caseInteger[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = 1},
        Return[dimlesscoef];
    ];


(*Ready*)
caseSymbol[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = 1, flag = 0, i},
        
        For[i = 1, i <= Length[variables], i++,
            If[variables[[i]] == term,
                dimlesscoef = variablescales[[i]];
                If[globalNoUnidentifiedSymbols == "No Unidentified Symbols",
                    If[variableKSAS[[i]] == True,
                        ,
                        dimlesscoef = 1;
                        ,
                        dimlesscoef = 1;
                    ];
                ];
                flag = 1;
                Break[];
            ];
        ];
        If[flag == 0,
            Print["CRITICAL ERROR: Did not identify a variables. Term is: ", term];
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
casePietrzykFlip[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, argument = term[[1]], superscript = term[[2]]},
        
        dimlesscoef = decider[argument, variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, graddenoters, \
          divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
          pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        
        Return[dimlesscoef];
    ];


(*Ready*)
casePower[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, argument = term[[1]], power = term[[2]]},
        
        dimlesscoef = decider[argument, variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, graddenoters, \
          divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
          pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option]^power;
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseDerivative[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = 1, argument = term[[1]], derivativeVariable = term[[2]], \
    argumentscale, derivativeVariablescale},
        
        (*Scale of Argument*)
        argumentscale = decider[argument, variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, graddenoters, \
          divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
          pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        
        (*Scale of derivatives*)
        derivativeVariablescale = decider[derivativeVariable, variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, graddenoters, \
          divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
          pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        
        dimlesscoef = argumentscale / derivativeVariablescale;
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseTimes[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, i, scalesvec = ConstantArray[0, Length[term]]},
        
        For[i = 1, i <= Length[term], i++,
            scalesvec[[i]] = decider[term[[i]], variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
              pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        ];
        dimlesscoef = listProduct[scalesvec];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseDot[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, i, scalesvec = ConstantArray[0, Length[term]]},
        
        For[i = 1, i <= Length[term], i++,
            scalesvec[[i]] = decider[term[[i]], variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
              pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        ];
        dimlesscoef = listProduct[scalesvec];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseTensorProduct[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, i, scalesvec = ConstantArray[0, Length[term]]},
        
        For[i = 1, i <= Length[term], i++,
            scalesvec[[i]] = decider[term[[i]], variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
              pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        ];
        dimlesscoef = listProduct[scalesvec];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseDoubleDot[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, i, scalesvec = ConstantArray[0, Length[term]]},
        
        For[i = 1, i <= Length[term], i++,
            scalesvec[[i]] = decider[term[[i]], variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
              pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        ];
        dimlesscoef = listProduct[scalesvec];
        
        Return[dimlesscoef];
    ];


(*Ready*)
casePlus[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{},
        Print["CRITICAL ERROR: casePlus: Not written yet. Term is: ", term];
        Return[Null];
    ];


(*Ready*)
caseGradient[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = 1, argument = term[[1]], derivativeVariable = term[[2]], i, \
    argumentscale, derivativeVariablescale},
        
        (*Scale of Argument*)
        argumentscale = decider[argument, variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, graddenoters, \
          divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
          pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        
        (*Scale of Gradient Variables*)
        derivativeVariablescale = ConstantArray[0, Length[derivativeVariable]];
        For[i = 1, i <= Length[derivativeVariable], i++,
            derivativeVariablescale[[i]] = decider[derivativeVariable[[i]], variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
              pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        ];
        
        If[Equal @@ derivativeVariablescale,
            derivativeVariablescale = derivativeVariablescale[[1]];
            dimlesscoef = argumentscale / derivativeVariablescale;
            Return[dimlesscoef];
            ,
            Print["CRITICAL ERROR: caseGradient: Not all independent variables in the gradient are scaled the same. This cannot be handled yet. Term is ", term];
            Return[Null];
            ,
            Print["CRITICAL ERROR: caseGradient: Not all independent variables in the gradient are scaled the same. This cannot be handled yet. Term is ", term];
            Return[Null];
        ];
        
    ];


(*Ready*)
caseDivergence[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = 1, argument = term[[1]], derivativeVariable = term[[2]], i, \
    argumentscale, derivativeVariablescale},
        
        (*Scale of Argument*)
        argumentscale = decider[argument, variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, graddenoters, \
          divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
          pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        
        (*Scale of Gradient Variables*)
        derivativeVariablescale = ConstantArray[0, Length[derivativeVariable]];
        For[i = 1, i <= Length[derivativeVariable], i++,
            derivativeVariablescale[[i]] = decider[derivativeVariable[[i]], variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
              pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        ];
        
        If[Equal @@ derivativeVariablescale,
            derivativeVariablescale = derivativeVariablescale[[1]];
            dimlesscoef = argumentscale / derivativeVariablescale;
            Return[dimlesscoef];
            ,
            Print["CRITICAL ERROR: caseDivergence: Not all independent variables in the divergence are scaled the same. This cannot be handled yet. Term is ", term];
            Return[Null];
            ,
            Print["CRITICAL ERROR: caseDivergence: Not all independent variables in the divergence are scaled the same. This cannot be handled yet. Term is ", term];
            Return[Null];
        ];
        
    ];


(*Ready*)
caseAverage[term_, variables_, variablescales_, variableKSAS_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = 1, argument = term[[1, 1]], subscript = term[[2]]},
        
        dimlesscoef = decider[argument, variables, variablescales, variableKSAS, scalars, vectors, tensors, closurevar, graddenoters, \
          divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
          pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        
        Return[dimlesscoef];
    ];


listProduct[x_List] :=
    Times @@ x;


End[]

EndPackage[]

