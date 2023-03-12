(* ::Package:: *)

(* Symbolica \[Copyright] 2023 by Kyle Pietrzyk is licensed under Attribution-NonCommercial-ShareAlike 4.0 International *)


(****** TermIVIncluder.wl -- 
*************)

(* 

TermIVIncluder[term, ] 

*)


(* Created January 27 2021
Modified : 
                       *)



BeginPackage["Disruptioneering`TermIVIncluder`"];

TermIVIncluder::usage = 
"TermIVIncluder[term, ] ";



Begin["`Disruptioneering`"];

(*avgdenoters should be {{Subscript[\[LeftAngleBracket]spaceholder\[RightAngleBracket], sub], sub, spaceholder},
{{{\[CapitalOmega],\[CapitalGamma]},{\[Xi],\[Eta]},{{\[CapitalOmega]a,\[CapitalOmega]b},{\[CapitalGamma]a,\[CapitalGamma]b}}},{{\[CapitalOmega]a,\[CapitalGamma]a},{\[Xi], \[Eta]},{{},{\[CapitalGamma]ai,\[CapitalGamma]ao}}},
{{\[CapitalOmega]b,\[CapitalGamma]b},{\[Xi], \[Eta]},{{},{\[CapitalGamma]bi,\[CapitalGamma]bo}}}{{\[CapitalDelta]\[Tau]PE1,\[Delta]\[Tau]PE1}, {\[Tau]Pe1},{{},{}}}}}*)
(*graddenoters should be {Grad[spaceholder, sub], sub, spaceholder}*)
(*divdenoters should be {Div[spaceholder, sub], sub, spaceholder}*)

TermIVIncluder[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, option_] := 
    procedure[term, terminclude, IVinclude, scalars, vectors, tensors, closurevar, \
      graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
      doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, option];


procedure[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, option_] :=
    Block[{functionName = "TermIVIncluder", solution},
        
        solution = main[Expand[term], terminclude, IVinclude, scalars, vectors, tensors, \
          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
          slowIV, fastIV, option];
        
        solution = Expand[solution];
        
        Return[solution];
    ];


main[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, option_] :=
    Block[{solution},
        
        If[Head[term] == List,
            solution = caseListmain[term, terminclude, IVinclude, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
              gradform, normalvecform, Uform, slowIV, fastIV, option];
            ,
            solution = caseNonListmain[term, terminclude, IVinclude, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
              gradform, normalvecform, Uform, slowIV, fastIV, option];
            ,
            solution = caseNonListmain[term, terminclude, IVinclude, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
              gradform, normalvecform, Uform, slowIV, fastIV, option];
        ];
        
        Return[solution];
    ];


caseListmain[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, option_] :=
    Block[{i, solution = ConstantArray[0, Length[term]]},
        
        For[i = 1, i <= Length[term], i++,
            solution[[i]] = main[term[[i]], terminclude, IVinclude, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
              gradform, normalvecform, Uform, slowIV, fastIV, option];
        ];
        
        Return[solution];
    ];


caseNonListmain[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, option_] :=
    Block[{i, solution, solutionvec},
                
        If[Head[term] == Plus,
            solutionvec = ConstantArray[0, Length[term]];
            For[i = 1, i <= Length[term], i++,
                solutionvec[[i]] = casePlusSubstituter[term[[i]], terminclude, IVinclude, scalars, \
                  vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  option];
            ];
            
            solution = Total[solutionvec];
            ,
            solution = casePlusSubstituter[term, terminclude, IVinclude, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
              gradform, normalvecform, Uform, slowIV, fastIV, option];
            ,
            solution = casePlusSubstituter[term, terminclude, IVinclude, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
              gradform, normalvecform, Uform, slowIV, fastIV, option];
        ];
        
        Return[solution];
    ];


casePlusSubstituter[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, option_] :=
    Block[{solution, levelcountdummy = 0},
        
        solution = decider[term, terminclude, IVinclude, scalars, vectors, tensors, closurevar, \
          graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
          normalvecform, Uform, slowIV, fastIV, levelcountdummy, {}];
        
        If[option == "Simplify",
            solution = getterminfo[solution, {}, scalars, vectors, tensors, closurevar, \
              graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
              doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, {}, {}];
            solution = MakeSingleTerm[solution, {}, dotdenoters, tensorproductdenoters, \
              doubledotdenoters][[1, 1, 1]];
        ];
        
        Return[solution];
    ];


decider[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef, levelcount = levelcountdummy + 1, chosen = 0, dependencies, i, \
    flagsubscript = 0},
        
        printouttoggle = 0;
        
        If[printouttoggle == 1,
            Print[term];
        ];
        If[levelcount == 50,
            
            Print[functionName <> ": decider: Reached levelcount limit. Backing out."];
            
            ,
            
            If[Head[term] == List && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified List"];                
                ];
                chosen = 1;
                dimlesscoef = caseList[term, terminclude, IVinclude, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Rational && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified Rational"];                
                ];
                chosen = 1;
                dimlesscoef = caseRational[term, terminclude, IVinclude, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            If[Head[term] == Integer && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified Integer"];                
                ];
                chosen = 1;
                dimlesscoef = caseInteger[term, terminclude, IVinclude, scalars, vectors, \
                  tensors,closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            If[Head[term] == Symbol && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified Symbol"];                
                ];
                chosen = 1;
                dimlesscoef = caseSymbol[term, terminclude, IVinclude, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            If[Head[term] == Head[pietrzykflipdenoters[[1]]] && chosen == 0,
                If[term[[2]] == pietrzykflipdenoters[[2]],
                    If[printouttoggle == 1,
                        Print[functionName <> ": decider: Identified Pietrzyk Flip"];                
                    ];
                    chosen = 1;
                    dimlesscoef = casePietrzykFlip[term, terminclude, IVinclude, scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                      gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                ];
            ];
            If[Head[term] == Power && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified Power"];                
                ];
                chosen = 1;
                dimlesscoef = casePower[term, terminclude, IVinclude, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[derivativedenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified Derivative"];                
                ];
                chosen = 1;
                dimlesscoef = caseDerivative[term, terminclude, IVinclude, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            If[Head[term] == Times && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified Times"];                
                ];
                chosen = 1;
                dimlesscoef = caseTimes[term, terminclude, IVinclude, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[dotdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified Dot"];                
                ];
                chosen = 1;
                dimlesscoef = caseDot[term, terminclude, IVinclude, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[tensorproductdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified TensorProduct"];                
                ];
                chosen = 1;
                dimlesscoef = caseTensorProduct[term, terminclude, IVinclude, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            If[Head[term] == Head[doubledotdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified DoubleDot"];                
                ];
                chosen = 1;
                dimlesscoef = caseDoubleDot[term, terminclude, IVinclude, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Plus && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified Plus"];                
                ];
                chosen = 1;
                dimlesscoef = casePlus[term, terminclude, IVinclude, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[graddenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified Gradient"];
                ];
                chosen = 1;
                dimlesscoef = caseGradient[term, terminclude, IVinclude, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            If[Head[term] == Head[divdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified Divergence"];
                ];
                chosen = 1;
                dimlesscoef = caseDivergence[term, terminclude, IVinclude, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            If[Head[term] == Head[avgdenoters[[1, 1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified Average"];
                ];
                chosen = 1;
                dimlesscoef = caseAverage[term, terminclude, IVinclude, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
            
            If[chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": decider: Identified None"];                
                ];
                If[Head[Head[term]] == Symbol,
                    dependencies = ConstantArray[0, Length[term]];
                    For[i = 1, i <= Length[term], i++,
                        dependencies[[i]] = term[[i]];
                    ];
                ];
                dimlesscoef = decider[Head[term], terminclude, IVinclude, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  dependencies];
            ];
            
            ,
            
            If[printouttoggle == 1,
                Print["CRITICAL ERROR: " <> functionName <> ": decider: Could not understand value of variable 'levelcount'."];
            ];
            
        ];
        
        If[printouttoggle == 1,
            Print[dimlesscoef];
        ];
        
        Return[dimlesscoef];
        
    ];


(*Ready*)
caseList[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{},
        Print["CRITICAL ERROR: " <> functionName <> ": caseList: Not written yet (but there shouldnt be any lists). Term is: ", term];
        Return[Null];
    ];


(*Ready*)
caseRational[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = term},
        Return[dimlesscoef];
    ];


(*Ready*)
caseInteger[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = term},
        Return[dimlesscoef];
    ];


(*Ready*)
caseSymbol[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, \
gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, i, ii, term2, newdependencies = option, term3, flag = 0, \
    j, chosen = 0},
        
        term2 = dependencyadder[term, option];
        
        For[i = 1, i <= Length[terminclude], i++,
            (*See if the term is one that should have its dependencies modified.*)
            If[terminclude[[i]] == term2,
                flag = 1;
                
                chosen = 0;
                If[Head[IVinclude[[1]]] == List,
                    chosen = 1;
                    For[ii = 1, ii <= Length[IVinclude], ii++,
                        If[Intersection[IVinclude[[ii, 1]], option] == IVinclude[[ii, 1]],
                            newdependencies = Join[newdependencies, IVinclude[[ii, 2]]];
                        ];
                    ];
                ];
                
                If[chosen == 0,
                    newdependencies = Join[newdependencies, IVinclude];
                ];
                
                Break[];
            ];
        ];
        
        dimlesscoef = dependencyadder[term, newdependencies];
        
        Return[dimlesscoef];
    ];
    


(*Ready*)
casePietrzykFlip[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, argument = term[[1]], power = term[[2]], inthepower},
        
        inthepower = decider[argument, terminclude, IVinclude, scalars, vectors, tensors, \
          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
          levelcountdummy, option];
        dimlesscoef = OperatorApplier[inthepower, pietrzykflipdenoters, power];
        
        Return[dimlesscoef];
    ];


(*Ready*)
casePower[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, argument = term[[1]], power = term[[2]], inthepower},
        
        inthepower = decider[argument, terminclude, IVinclude, scalars, vectors, tensors, \
          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
          levelcountdummy, option];
        dimlesscoef = inthepower^power;
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseDerivative[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, subscript = term[[2]], argument = term[[1]], newargument},
        
        newargument = decider[argument, terminclude, IVinclude, scalars, vectors, \
          tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
          levelcountdummy, option];
        dimlesscoef = OperatorApplier[newargument, derivativedenoters, subscript];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseTimes[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, productvec = ConstantArray[0, Length[term]], i},
        
        For[i = 1, i <= Length[term], i++,
            productvec[[i]] = decider[term[[i]], terminclude, IVinclude, scalars, vectors, \
              tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
        ];
        dimlesscoef = listProduct[productvec];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseDot[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, i, dotvec = ConstantArray[0, Length[term]]},
        
        For[i = 1, i <= Length[term], i++,
            dotvec[[i]] = decider[term[[i]], terminclude, IVinclude, scalars, vectors, \
              tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
        ];
        
        For[i = 1, i <= Length[dotvec], i++,
            If[i == 1,
                dimlesscoef = dotvec[[i]];
                ,
                dimlesscoef = OperatorApplier[dimlesscoef, dotdenoters, dotvec[[i]]];
            ];
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseTensorProduct[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, i, tensorvec = ConstantArray[0, Length[term]]},
        
        For[i = 1, i <= Length[term], i++,
            tensorvec[[i]] = decider[term[[i]], terminclude, IVinclude, scalars, vectors, \
              tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
        ];
        
        For[i = 1, i <= Length[tensorvec], i++,
            If[i == 1,
                dimlesscoef = tensorvec[[i]];
                ,
                dimlesscoef = OperatorApplier[dimlesscoef, tensorproductdenoters, \
                  tensorvec[[i]]];
            ];
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseDoubleDot[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, i, doubledotvec = ConstantArray[0, Length[term]]},
        
        For[i = 1, i <= Length[term], i++,
            doubledotvec[[i]] = decider[term[[i]], terminclude, IVinclude, scalars, vectors, \
              tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, levelcountdummy, option];
        ];
        
        For[i = 1, i <= Length[doubledotvec], i++,
            If[i == 1,
                dimlesscoef = doubledotvec[[i]];
                ,
                dimlesscoef = OperatorApplier[dimlesscoef, doubledotdenoters, \
                  doubledotvec[[i]]];
            ];
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
casePlus[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = 0, i},
        
        For[i = 1, i <= Length[term], i++,
            dimlesscoef += decider[term[[i]], terminclude, IVinclude, scalars, vectors, \
              tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseGradient[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, subscript = term[[2]], argument = term[[1]], newargument},
        
        newargument = decider[argument, terminclude, IVinclude, scalars, vectors, \
          tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
          levelcountdummy, option];
        dimlesscoef = OperatorApplier[newargument, graddenoters, subscript];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseDivergence[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, subscript = term[[2]], argument = term[[1]], newargument},
        
        newargument = decider[argument, terminclude, IVinclude, scalars, vectors, \
          tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
          levelcountdummy, option];
        dimlesscoef = OperatorApplier[newargument, divdenoters, subscript];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseAverage[term_, terminclude_, IVinclude_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, subscript = term[[2]], argument = term[[1, 1]], newargument},
        
        newargument = decider[argument, terminclude, IVinclude, scalars, vectors, \
          tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
          levelcountdummy, option];
        dimlesscoef = OperatorApplier[newargument, avgdenoters[[1]], subscript];
        
        Return[dimlesscoef];
    ];


(*Checked*)
getterminfo[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_, option2_] :=
    Block[{dimlesscoef, globalSimplificationMode = option2, \
    globalSimplificationDerivatives = {"Distribute Derivatives"}, \
    globalSimplificationDots = {"Simplify Dot Product"}, \
    globalSimplificationTensorProducts = {"Simplify Tensor Product"}, \
    globalSimplificationDoubleDots = {"Simplify Double Dot Product"}, \
    globalSimplificationGradients = {"Distribute Gradients"}, \
    globalSimplificationDivergences = {"Distribute Divergences"}},
        
        If[globalSimplificationMode == {"All"},
            globalSimplificationMode = {"Simplify Dot Product", "Distribute Divergences", \
              "Simplify Tensor Product", "Distribute Derivatives", "Distribute Gradients", \
              "Simplify Double Dot Product"};
        ];
        
        dimlesscoef = getterminfo[term, isdependenton, scalars, vectors, tensors, closurevar, graddenoters, \
          divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
          doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
          slowIV, fastIV, levelcountdummy, option];
        
        Return[dimlesscoef];
    ];


(*Checked*)
getterminfo[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, levelcount = levelcountdummy + 1, chosen = 0, \
    dependencies, i, flagsubscript = 0, printouttoggle = 0},
        
        If[printouttoggle == 1,
            Print[term];
        ];
        If[levelcount == 1000,
            
            Print["CRITICAL ERROR: " <> functionName <> ": getterminfo: Reached levelcount limit. Backing out."];
            
            ,
            
            If[Head[term] == List && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified List"];
                ];
                chosen = 1;
                dimlesscoef = subsubcaseList[term, isdependenton, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, option];
            ];
            If[Head[term] == Rational && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified Rational"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseRational[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, option];
            ];
            If[Head[term] == Integer && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified Integer"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseInteger[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, option];
            ];
            If[Head[term] == Symbol && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified Symbol"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseSymbol[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, option];
            ];
            If[Head[term] == Head[pietrzykflipdenoters[[1]]] && chosen == 0,
                If[term[[2]] == pietrzykflipdenoters[[2]],
                    If[printouttoggle == 1,
                        Print[functionName <> ": getterminfo: Identified Pietrzyk Flip"];                
                    ];
                    chosen = 1;
                    dimlesscoef = subsubcasePietrzykFlip[term, isdependenton, scalars, \
                      vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, \
                      derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcount, option];
                ];
            ];
            If[Head[term] == Power && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified Power"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcasePower[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, option];
            ];
            If[Head[term] == Head[derivativedenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified Derivative"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseDerivative[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, option];
            ];
            If[Head[term] == Times && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified Times"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseTimes[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, option];
            ];
            If[Head[term] == Head[dotdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified Dot"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseDot[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, option];
            ];
            If[Head[term] == Head[tensorproductdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified TensorProduct"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseTensorProduct[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, option];
            ];
            If[Head[term] == Head[doubledotdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified DoubleDot"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseDoubleDot[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, option];
            ];
            If[Head[term] == Plus && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified Plus"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcasePlus[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, option];
            ];
            If[Head[term] == Head[graddenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified Gradient"];
                ];
                chosen = 1;
                dimlesscoef = subsubcaseGradient[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, option];
            ];
            If[Head[term] == Head[divdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified Divergence"];
                ];
                chosen = 1;
                dimlesscoef = subsubcaseDivergence[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, option];
            ];
            If[Head[term] == Head[avgdenoters[[1, 1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified Average"];
                ];
                chosen = 1;
                dimlesscoef = subsubcaseAverage[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, option];
            ];
            
            If[chosen == 0,
                If[printouttoggle == 1,
                    Print[functionName <> ": getterminfo: Identified None"];                
                ];
                If[Head[Head[term]] == Symbol,
                    dependencies = ConstantArray[0, Length[term]];
                    For[i = 1, i <= Length[term], i++,
                        dependencies[[i]] = term[[i]];
                    ];
                ];
                dimlesscoef = getterminfo[Head[term], isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcount, dependencies];
            ];
            
            ,
            
            If[printouttoggle == 1,
                Print["CRITICAL ERROR: " <> functionName <> ": getterminfo: Could not understand value of variable 'levelcount'."];                
            ];
            
        ];
        
        If[printouttoggle == 1,
            Print[dimlesscoef];
        ];
        
        Return[dimlesscoef];
        
    ];


(*Checked*)
subsubcaseList[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{},
        Print["CRITICAL ERROR: " <> functionName <> ": subsubcaseList: Not written yet (but there shouldnt be any lists). Term is: ", term];
        Return[Null];
    ];


(*Checked*)
subsubcaseRational[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{term}, {0}, {0}}}},
        Return[dimlesscoef];
    ];


(*Checked*)
subsubcaseInteger[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{term}, {0}, {0}}}},
        Return[dimlesscoef];
    ];


(*Checked*)
subsubcaseSymbol[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{}, {}, {}}}, i, j, fullterm, term2, scalvec = -1, \
    dummy = ConstantArray[0, Length[isdependenton]], scalvecten},
        
        If[option == {},
            
            (*Assign value*)
            dimlesscoef[[1, 1]] = {term};
            
            (*Check for tensor, vector, or scalar*)
            scalvecten = -1;
            For[i = 1, i <= Length[tensors], i++,
                If[tensors[[i]] == term,
                    scalvecten = 2;
                    Break[];
                ];
            ];
            If[scalvecten == -1,
                For[i = 1, i <= Length[vectors], i++,
                    If[vectors[[i]] == term,
                        scalvecten = 1;
                        Break[];
                    ];
                ];
            ];
            If[scalvecten == -1,
                For[i = 1, i <= Length[scalars], i++,
                    If[scalars[[i]] == term,
                        scalvecten = 0;
                        Break[];
                    ];
                ];
            ];
            If[scalvecten == -1,
               Print[functionName <> ": subsubcaseSymbol: Variable unidentified as a tensor, vector, or scalar. Procedure will continue assuming it is a scalar. Variable is: ", term];
               dimlesscoef[[1, 2]] = {0};
               ,
               dimlesscoef[[1, 2]] = {scalvecten};
            ];
            
            (*Check dependency*)
            dimlesscoef[[1, 3]] = {0};
            
            
            ,
            
            
            (*Assign value*)
            term2 = dependencyadder[term, option];
            dimlesscoef[[1, 1]] = {term2};
            
            (*Check for tensor, vector, or scalar*)
            scalvecten = -1;
            For[i = 1, i <= Length[tensors], i++,
                If[tensors[[i]] == term2,
                    scalvecten = 2;
                    Break[];
                ];
            ];
            If[scalvecten == -1,
                For[i = 1, i <= Length[vectors], i++,
                    If[vectors[[i]] == term2,
                        scalvecten = 1;
                        Break[];
                    ];
                ];
            ];
            If[scalvecten == -1,
                For[i = 1, i <= Length[scalars], i++,
                    If[scalars[[i]] == term2,
                        scalvecten = 0;
                        Break[];
                    ];
                ];
            ];
            If[scalvecten == -1,
               Print[functionName <> ": subsubcaseSymbol: Variable unidentified as a tensor, vector, or scalar. Procedure will continue assuming it is a scalar. Variable is: ", term2];
               dimlesscoef[[1, 2]] = {0};
               ,
               dimlesscoef[[1, 2]] = {scalvecten};
            ];
            
            (*Check dependency*)
            For[j = 1, j <= Length[isdependenton], j++,
                For[i = 1, i <= Length[option], i++,
                    If[option[[i]] == isdependenton[[j]],
                        dummy[[j]] += 1;
                        Break[];
                    ];
                ];
            ];
            If[Total[dummy] == Length[isdependenton],
                dimlesscoef[[1, 3]] = {1};
                ,
                If[Total[dummy] == 0,
                    dimlesscoef[[1, 3]] = {0};
                    ,
                    Print[functionName <> ": subsubcaseSymbol: A variable was found to not dependent on all independent variables listed in 'isdependenton'. Procedure will continue assuming it is a function of the independent variables, in general. Variable is: ", term2];
                    dimlesscoef[[1, 3]] = {1};
                ];
            ];
            
            
            ,
            
            
            Print["CRITICAL ERROR: " <> functionName <> ": subsubcaseSymbol: Variable 'option' cannot be identified. Term is: ", term2];
            Return[Null];
            
        ];
        
        Return[dimlesscoef];
    ];


(*Checked*)
subsubcasePietrzykFlip[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, argument = term[[1]], superscript = term[[2]], inthepower, i, j, \
    expandedterm, expandedtermparts, newinthepower = {{{}, {}, {}}}, chosen2 = 0, \
    dimlesscoefparts, bracketeddummy, iii, dummy, scalarmults = {{{}, {}, {}}}},
        
        expandedterm = Expand[argument];
        
        If[Head[expandedterm] == Plus,
            For[i = 1, i <= Length[expandedterm], i++,
                bracketeddummy = OperatorApplier[expandedterm[[i]], pietrzykflipdenoters, \
                  superscript];
                
                dimlesscoefparts = getterminfo[bracketeddummy, isdependenton, scalars, \
                  vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                dimlesscoef = Join[dimlesscoef, dimlesscoefparts];
            ];
            
            Return[dimlesscoef];
        ];
        
        
        
        
        
        inthepower = getterminfo[argument, isdependenton, scalars, vectors, tensors, \
          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
          normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        
        For[iii = 1, iii <= Length[inthepower], iii++,
            chosen2 = 0;
            
            dummy = MakeSingleTerm[{inthepower[[iii]]}, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            If[dummy[[1, 1, 1]] == 0 && chosen2 == 0,
                chosen2 = 1;
            ];
            
            If[chosen2 == 0,
                scalarmults = {{{}, {}, {}}};
                newinthepower = {{{}, {}, {}}};
                For[i = 1, i <= Length[inthepower[[iii, 1]]], i++,
                    If[inthepower[[iii, 2, i]] == 0,
                        scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], \
                          {inthepower[[iii, 1, i]]}];
                        scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], \
                          {inthepower[[iii, 2, i]]}];
                        scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], \
                          {inthepower[[iii, 3, i]]}];
                        ,
                        newinthepower[[1, 1]] = Join[newinthepower[[1, 1]], \
                          {inthepower[[iii, 1, i]]}];
                        newinthepower[[1, 2]] = Join[newinthepower[[1, 2]], \
                          {inthepower[[iii, 2, i]]}];
                        newinthepower[[1, 3]] = Join[newinthepower[[1, 3]], \
                          {inthepower[[iii, 3, i]]}];
                    ];
                ];
                dummy = MakeSingleTerm[newinthepower, {}, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters];
                dummy[[1, 1, 1]] = OperatorApplier[dummy[[1, 1, 1]], pietrzykflipdenoters, \
                  superscript];
                dummy[[1, 1]] = Join[scalarmults[[1, 1]], dummy[[1, 1]]];
                dummy[[1, 2]] = Join[scalarmults[[1, 2]], dummy[[1, 2]]];
                dummy[[1, 3]] = Join[scalarmults[[1, 3]], dummy[[1, 3]]];
                
                dimlesscoef = Join[dimlesscoef, dummy];
            ];
        ];
        
        (*In case 'inthepower' is just a sum of zeros.*)
        If[dimlesscoef == {},
            inthepower = MakeSingleTerm[inthepower, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            dimlesscoef = {{{0}, {inthepower[[1, 2, 1]]}, {0}}};
        ];
        
        Return[dimlesscoef];
    ];


(*Checked*)
subsubcasePower[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, argument = term[[1]], power = term[[2]], inthepower, i, j, \
    expandedterm = Expand[term], expandedtermparts, newinthepower = 0, \
    chosen2 = 0, dimlesscoefparts, bracketeddummy},
        
        If[Head[expandedterm] == Plus,
            For[i = 1, i <= Length[expandedterm], i++,
                bracketeddummy = expandedterm[[i]];
                
                dimlesscoefparts = getterminfo[bracketeddummy, isdependenton, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                dimlesscoef = Join[dimlesscoef, dimlesscoefparts];
            ];
            
            Return[dimlesscoef];
        ];
        
        
        
        
        
        inthepower = getterminfo[argument, isdependenton, scalars, vectors, tensors, \
          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
          normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        
        chosen2 = 0;
        If[Length[inthepower] > 1,
            chosen2 = 1;
            newinthepower = MakeSingleTerm[inthepower, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            If[power <= 0,
                newinthepower[[1, 1, 1]] = Expand[newinthepower[[1, 1, 1]]^power];
                dimlesscoef = newinthepower;
                ,
                newinthepower = Expand[newinthepower[[1, 1, 1]]^power];
                dimlesscoef = getterminfo[newinthepower, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
                ,
                newinthepower = Expand[newinthepower[[1, 1, 1]]^power];
                dimlesscoef = getterminfo[newinthepower, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
            ];
        ];
        If[Length[inthepower] == 1 && chosen2 == 0,
            chosen2 = 1;
            dimlesscoef = inthepower;
            For[i = 1, i <= Length[dimlesscoef[[1, 1]]], i++,
                dimlesscoef[[1, 1, i]] = dimlesscoef[[1, 1, i]]^power;
            ];
        ];
        If[chosen2 == 0,
            Print["CRITICAL ERROR: " <> functionName <> ": subsubcasePower: Unable to identify the number of terms coming back from 'getterminfo'. Term is: ", term];
            Return[Null];
        ];
        
        Return[dimlesscoef];
    ];


(*Checked*)
subsubcaseDerivative[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, subscript = term[[2]], argument = term[[1]], \
    flag = 0, argumentparts, i, expandedargument, dimlesscoefparts, iii, bracketeddummy, \
    argumentpartsbracketed},
        
        expandedargument = Expand[argument];
        
        If[Head[expandedargument] == Plus,
            For[i = 1, i <= Length[expandedargument], i++,
                bracketeddummy = OperatorApplier[expandedargument[[i]], derivativedenoters, \
                  subscript];
                
                dimlesscoefparts = getterminfo[bracketeddummy, isdependenton, scalars, \
                  vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                dimlesscoef = Join[dimlesscoef, dimlesscoefparts];
            ];
            
            Return[dimlesscoef];
        ];
        
        
        
        
        
        (*Get information on the argument.*)
        argumentparts = getterminfo[argument, {subscript}, scalars, vectors, tensors, \
          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
          slowIV, fastIV, levelcountdummy, option];
        
        (*Analyze and simplify the information of each summed component individually.*)
        For[iii = 1, iii <= Length[argumentparts], iii++,
            argumentpartsbracketed = subsubcaseDerivativeSimp[{argumentparts[[iii]]}, \
              subscript, isdependenton, scalars, vectors, tensors, closurevar, \
              graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
              doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
            For[i = 1, i <= Length[argumentpartsbracketed], i++,
                If[listProduct[argumentpartsbracketed[[i, 1]]] == 0,
                    ,
                    dimlesscoef = Join[dimlesscoef, {argumentpartsbracketed[[i]]}];
                    ,
                    dimlesscoef = Join[dimlesscoef, {argumentpartsbracketed[[i]]}];
                ];
            ];
        ];
        
        (*Account for zeros.*)
        If[dimlesscoef == {},
            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            dimlesscoef = {{{0}, {argumentparts[[1, 2, 1]]}, {0}}};
        ];
        
        Return[dimlesscoef];
    ];


(*Checked*)
subsubcaseDerivativeSimp[argumentparts_, subscript_, isdependenton_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, \
dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, \
normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{}, {}, {}}}, scalarmults = {{{}, {}, {}}}, \
    newargumentparts = {{{}, {}, {}}}, i, flag1 = 0, flag2 = 0, chosen = 0, \
    newargumentparts2, scalarmults2, dummyargumentparts, dummynewargumentparts, \
    count = 0, chosen2 = 0, dependentdotterm, iterms, imult, icombine},
        
        (*Take out the scalars that arent functions of the appropriate independent variable.*)
        For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
            If[argumentparts[[1, 3, i]] == 0,
                scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], {argumentparts[[1, 1, i]]}];
                scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], {argumentparts[[1, 2, i]]}];
                scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], {argumentparts[[1, 3, i]]}];
                ,
                newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                  {argumentparts[[1, 1, i]]}];
                newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                  {argumentparts[[1, 2, i]]}];
                newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                  {argumentparts[[1, 3, i]]}];
            ];
        ];
        
        (*Check to see if there is anything with the correct dependency in the derivative.*)
        If[Total[newargumentparts[[1, 3]]] == 0,
            dimlesscoef = {{{0}, {Total[argumentparts[[1, 2]]]}, {0}}};
            Return[dimlesscoef];
        ];
        
        
        (*Decide the new dependency*)
        (*If the derivative independent variable is the same as 'isdependenton'.*)
        If[Intersection[{subscript}, isdependenton] != {},
            chosen = 1;
            
            If[Length[newargumentparts[[1, 1]]] > 0,
                newargumentparts = subsubcaseDerivativeSimpSimp[newargumentparts, \
                  subscript, isdependenton, scalars, vectors, tensors, closurevar, \
                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
                ,
                dimlesscoef = {{{0}, {Total[argumentparts[[1, 2]]]}, {0}}};
                Return[dimlesscoef];
            ];
            
            
            dimlesscoef = {};
            For[icombine = 1, icombine <= Length[newargumentparts], icombine++,
                For[imult = 1, imult <= Length[scalarmults], imult++,
                    dimlesscoef = Join[dimlesscoef, {{{}, {}, {}}}];
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      newargumentparts[[icombine, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      newargumentparts[[icombine, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      newargumentparts[[icombine, 3]]];
                    
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      scalarmults[[imult, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      scalarmults[[imult, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      scalarmults[[imult, 3]]];
                ];
            ];
            
        ];
        
        (*If the derivative independent variable is different than 'isdependenton'.*)
        If[chosen == 0,
            If[Length[scalarmults[[1, 1]]] > 0,
                scalarmults2 = getterminfo[listProduct[scalarmults[[1, 1]]], \
                  isdependenton, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  levelcountdummy, option];
                ,
                scalarmults2 = {{{}, {}, {}}};
            ];
            
            If[Length[newargumentparts[[1, 1]]] > 0,
                newargumentparts2 = getterminfo[listProduct[newargumentparts[[1, 1]]], \
                  isdependenton, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  levelcountdummy, option];
                
                newargumentparts2 = subsubcaseDerivativeSimpSimp[newargumentparts2, \
                  subscript, isdependenton, scalars, vectors, tensors, closurevar, \
                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  levelcountdummy, option];
                ,
                dimlesscoef = {{{0}, {Total[argumentparts[[1, 2]]]}, {0}}};
                Return[dimlesscoef];
            ];
            
            
            dimlesscoef = {};
            For[icombine = 1, icombine <= Length[newargumentparts2], icombine++,
                For[imult = 1, imult <= Length[scalarmults2], imult++,
                    dimlesscoef = Join[dimlesscoef, {{{}, {}, {}}}];
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      newargumentparts2[[icombine, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      newargumentparts2[[icombine, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      newargumentparts2[[icombine, 3]]];
                    
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      scalarmults2[[imult, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      scalarmults2[[imult, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      scalarmults2[[imult, 3]]];
                ];
            ];
            
        ];
        
        Return[dimlesscoef];
    ];    


(*Checked*)
subsubcaseDerivativeSimpSimp[term_, subscript_, isdependenton_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, \
dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, \
normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{i, chosen, iterms, newargumentparts2dummy, newargumentparts2 = {}},
        
        For[iterms = 1, iterms <= Length[term], iterms++,
            
            chosen = 0;
            
            If[Intersection[globalSimplificationMode, globalSimplificationDerivatives] != {},
                {newargumentparts2dummy, chosen} = subsubcaseDerivativeSimpSimpSimp[{term[[iterms]]}, subscript, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
                  slowIV, fastIV, levelcountdummy, option];
            ];
            
            If[chosen == 0,
                newargumentparts2 = Join[newargumentparts2, {term[[iterms]]}];
                
                (*Log the term*)
                newargumentparts2[[-1, 1]] = {OperatorApplier[listProduct[ \
                  newargumentparts2[[-1, 1]]], derivativedenoters, subscript]};
                
                (*Adjust the tensor rank*)
                newargumentparts2[[-1, 2]] = {Total[newargumentparts2[[-1, 2]]]};
                
                (*Assign Dependency*)
                If[Total[newargumentparts2[[-1, 3]]] >= 1,
                    newargumentparts2[[-1, 3]] = {1};
                    ,
                    newargumentparts2[[-1, 3]] = {0};
                ];
                
                ,
                
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
                
                ,
                
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
            ];
            
        ];
        
        Return[newargumentparts2];
    ];


(*Checked*)
subsubcaseDerivativeSimpSimpSimp[term_, subscript_, isdependenton_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, \
dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, \
normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{i, chosen = 0, newargumentparts2 = {}, flag1 = 0, flag2 = 0, scalar, \
    vector, termdotunfold, chosen2 = 0, chosen3 = 0, term2, termdotunfold2, divsym, \
    derivargument, izerocheck},
        
        (*If there is a zero in the term.*)
        For[izerocheck = 1, izerocheck <= Length[term[[1, 1]]], izerocheck++,
            If[term[[1, 1, izerocheck]] == 0 && chosen == 0,
                chosen = 1;
                
                newargumentparts2 = Join[newargumentparts2, term];
                
                (*Log the term*)
                newargumentparts2[[-1, 1]] = {0};
                
                (*Adjust scalar, vector, or tensor indication*)
                newargumentparts2[[-1, 2]] = {Total[newargumentparts2[[-1, 2]]]};
                
                (*Assign Dependency*)
                newargumentparts2[[-1, 3]] = {0};
                
                Break[];
            ];
        ];
        
        (*If the argument of the derivative is a dot product, the derivative can be 
        distributed.*)
        If[Length[term[[1, 1]]] == 1 && chosen == 0,
            If[Head[term[[1, 1, 1]]] == Head[dotdenoters[[1]]],
                chosen = 1;
                
                termdotunfold = UnfoldMergedTerms[term, dotdenoters, \
                  {subscript}, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                  fastIV, levelcountdummy, option];
                
                (*Zero Check*)
                chosen2 = 0;
                For[i = 1, i <= Length[termdotunfold[[1, 1, 1]]], i++,
                    If[termdotunfold[[1, 1, 1, i]] == 0,
                        chosen2 = 1;
                        newargumentparts2 = Join[newargumentparts2, {{{0}, {Total[term[[1, 2]]]}, {0}}}];
                        Break[];
                    ];
                ];
                
                If[chosen2 == 0,
                    termdotunfold2 = termdotunfold;
                    
                    For[i = 1, i <= Length[termdotunfold2[[1, 1, 1]]], i++,
                        If[termdotunfold[[1, 3, 1, i]] == 1,
                            termdotunfold[[1, 1, 1, i]] = OperatorApplier[ \
                              termdotunfold[[1, 1, 1, i]], derivativedenoters, subscript];
                            
                            termdotunfold = MakeSingleTerm[termdotunfold, dotdenoters, \
                              dotdenoters, tensorproductdenoters, doubledotdenoters];
                            
                            term2 = getterminfo[termdotunfold[[1, 1, 1]], \
                              isdependenton, scalars, vectors, tensors, closurevar, \
                              graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                              dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                              divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                              levelcountdummy, option];
                            
                            newargumentparts2 = Join[newargumentparts2, term2];
                            termdotunfold = termdotunfold2;
                        ];
                    ];
                ];
                
            ];
        ];
        
        (*If the argument of the derivative is a double dot product, the derivative can be 
        distributed.*)
        If[Length[term[[1, 1]]] == 1 && chosen == 0,
            If[Head[term[[1, 1, 1]]] == Head[doubledotdenoters[[1]]],
                chosen = 1;
                
                termdotunfold = UnfoldMergedTerms[term, doubledotdenoters, \
                  {subscript}, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                  fastIV, levelcountdummy, option];
                
                (*Zero Check*)
                chosen2 = 0;
                For[i = 1, i <= Length[termdotunfold[[1, 1, 1]]], i++,
                    If[termdotunfold[[1, 1, 1, i]] == 0,
                        chosen2 = 1;
                        newargumentparts2 = Join[newargumentparts2, {{{0}, {Total[term[[1, 2]]]}, {0}}}];
                        Break[];
                    ];
                ];
                
                If[chosen2 == 0,
                    termdotunfold2 = termdotunfold;
                    
                    For[i = 1, i <= Length[termdotunfold[[1, 1, 1]]], i++,
                        If[termdotunfold[[1, 3, 1, i]] == 1,
                            termdotunfold[[1, 1, 1, i]] = OperatorApplier[ \
                              termdotunfold[[1, 1, 1, i]], derivativedenoters, subscript];
                            
                            termdotunfold = MakeSingleTerm[termdotunfold, doubledotdenoters, \
                              dotdenoters, tensorproductdenoters, doubledotdenoters];
                            
                            term2 = getterminfo[termdotunfold[[1, 1, 1]], \
                              isdependenton, scalars, vectors, tensors, closurevar, \
                              graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                              dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                              divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                              levelcountdummy, option];
                            
                            newargumentparts2 = Join[newargumentparts2, term2];
                            termdotunfold = termdotunfold2;
                        ];
                    ];
                ];
                
            ];
        ];
        
        If[Length[term[[1, 1]]] == 1 && chosen == 0,
            If[Head[term[[1, 1, 1]]] == Head[divdenoters[[1]]],
                divsym = term[[1, 1, 1, 2]];
                If[Intersection[divsym, {subscript}] == {},
                    chosen = 1;
                    newargumentparts2 = Join[newargumentparts2, term];
                    
                    (*Log the term*)
                    derivargument = OperatorApplier[term[[1, 1, 1, 1]], \
                      derivativedenoters, subscript];
                    newargumentparts2[[-1, 1]] = {OperatorApplier[derivargument, \
                      divdenoters, divsym]};
                    
                    (*Adjust tensor rank*)
                    newargumentparts2[[-1, 2]] = {term[[1, 2, 1]]};
                    
                    (*Assign Dependency*)
                    newargumentparts2[[-1, 3]] = {term[[1, 3, 1]]};
                ];
            ];
        ];
        
        If[newargumentparts2 == {},
            newargumentparts2 = term;
        ];
        
        Return[{newargumentparts2, chosen}];
    ];


subsubcaseDerivativeSimpSimpOLD[term_, subscript_, isdependenton_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, \
dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, \
normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{i, chosen = 0, iterms, newargumentparts2 = {}, flag1 = 0, flag2 = 0, scalar, \
    vector, termdotunfold, chosen2 = 0, chosen3 = 0, term2, termdotunfold2, divsym, \
    derivargument, izerocheck},
        
        For[iterms = 1, iterms <= Length[term], iterms++,
            chosen = 0;
            
            (*If there is a zero in the term.*)
            For[izerocheck = 1, izerocheck <= Length[term[[iterms, 1]]], izerocheck++,
                If[term[[iterms, 1, izerocheck]] == 0 && chosen == 0,
                    chosen = 1;
                    
                    newargumentparts2 = Join[newargumentparts2, {term[[iterms]]}];
                    
                    (*Log the term*)
                    newargumentparts2[[-1, 1]] = {0};
                    
                    (*Adjust scalar, vector, or tensor indication*)
                    newargumentparts2[[-1, 2]] = {Total[newargumentparts2[[-1, 2]]]};
                    
                    (*Assign Dependency*)
                    newargumentparts2[[-1, 3]] = {0};
                    
                    Break[];
                ];
            ];
            
            (*If the argument of the derivative is a dot product, the derivative can be 
            distributed.*)
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == Head[dotdenoters[[1]]],
                    chosen = 1;
                    
                    termdotunfold = UnfoldMergedTerms[{term[[iterms]]}, dotdenoters, \
                      {subscript}, scalars, vectors, tensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
                    (*Zero Check*)
                    chosen2 = 0;
                    For[i = 1, i <= Length[termdotunfold[[1, 1, 1]]], i++,
                        If[termdotunfold[[1, 1, 1, i]] == 0,
                            chosen2 = 1;
                            newargumentparts2 = Join[newargumentparts2, {{{0}, {Total[term[[iterms, 2]]]}, {0}}}];
                            Break[];
                        ];
                    ];
                    
                    If[chosen2 == 0,
                        termdotunfold2 = termdotunfold;
                        
                        For[i = 1, i <= Length[termdotunfold2[[1, 1, 1]]], i++,
                            If[termdotunfold[[1, 3, 1, i]] == 1,
                                termdotunfold[[1, 1, 1, i]] = OperatorApplier[ \
                                  termdotunfold[[1, 1, 1, i]], derivativedenoters, subscript];
                                
                                termdotunfold = MakeSingleTerm[termdotunfold, dotdenoters, \
                                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                                
                                term2 = getterminfo[termdotunfold[[1, 1, 1]], \
                                  isdependenton, scalars, vectors, tensors, closurevar, \
                                  graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                                  divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                                  levelcountdummy, option];
                                
                                newargumentparts2 = Join[newargumentparts2, term2];
                                termdotunfold = termdotunfold2;
                            ];
                        ];
                    ];
                    
                ];
            ];
            
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == Head[doubledotdenoters[[1]]],
                    chosen = 1;
                    
                    termdotunfold = UnfoldMergedTerms[{term[[iterms]]}, doubledotdenoters, \
                      {subscript}, scalars, vectors, tensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
                    (*Zero Check*)
                    chosen2 = 0;
                    For[i = 1, i <= Length[termdotunfold[[1, 1, 1]]], i++,
                        If[termdotunfold[[1, 1, 1, i]] == 0,
                            chosen2 = 1;
                            newargumentparts2 = Join[newargumentparts2, {{{0}, {Total[term[[iterms, 2]]]}, {0}}}];
                            Break[];
                        ];
                    ];
                    
                    If[chosen2 == 0,
                        termdotunfold2 = termdotunfold;
                        
                        For[i = 1, i <= Length[termdotunfold[[1, 1, 1]]], i++,
                            If[termdotunfold[[1, 3, 1, i]] == 1,
                                termdotunfold[[1, 1, 1, i]] = OperatorApplier[ \
                                  termdotunfold[[1, 1, 1, i]], derivativedenoters, subscript];
                                
                                termdotunfold = MakeSingleTerm[termdotunfold, doubledotdenoters, \
                                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                                
                                term2 = getterminfo[termdotunfold[[1, 1, 1]], \
                                  isdependenton, scalars, vectors, tensors, closurevar, \
                                  graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                                  divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                                  levelcountdummy, option];
                                
                                newargumentparts2 = Join[newargumentparts2, term2];
                                termdotunfold = termdotunfold2;
                            ];
                        ];
                    ];
                    
                ];
            ];
            
            (*If argument of the derivative is a divergence, where the derivative being applied
            can move passed the divergence and onto the corresponding argument.*)
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == Head[divdenoters[[1]]],
                    divsym = term[[iterms, 1, 1, 2]];
                    If[Intersection[divsym, {subscript}] == {},
                        chosen = 1;
                        newargumentparts2 = Join[newargumentparts2, {term[[iterms]]}];
                        
                        (*Log the term*)
                        derivargument = OperatorApplier[term[[iterms, 1, 1, 1]], \
                          derivativedenoters, subscript];
                        newargumentparts2[[-1, 1]] = {OperatorApplier[derivargument, \
                          divdenoters, divsym]};
                        
                        (*Adjust tensor rank*)
                        newargumentparts2[[-1, 2]] = {term[[iterms, 2, 1]]};
                        
                        (*Assign Dependency*)
                        newargumentparts2[[-1, 3]] = {term[[iterms, 3, 1]]};
                    ];
                ];
            ];
            
            
            If[chosen == 0,
                newargumentparts2 = Join[newargumentparts2, {term[[iterms]]}];
                
                (*Log the term*)
                newargumentparts2[[-1, 1]] = {OperatorApplier[listProduct[ \
                  newargumentparts2[[-1, 1]]], derivativedenoters, subscript]};
                
                (*Adjust the tensor rank*)
                newargumentparts2[[-1, 2]] = {Total[newargumentparts2[[-1, 2]]]};
                
                (*Assign Dependency*)
                If[Total[newargumentparts2[[-1, 3]]] >= 1,
                    newargumentparts2[[-1, 3]] = {1};
                    ,
                    newargumentparts2[[-1, 3]] = {0};
                ];
            ];
            
        ];
        
        Return[newargumentparts2];
    ];


(*Checked*)
subsubcaseTimes[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{}, {}, {}}}, expandedterm = Expand[term], i, j, dummy, \
    camebackSum = {}, newterm},
    
        If[Head[expandedterm] == Plus,
            
            dimlesscoef = getterminfo[expandedterm, isdependenton, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, levelcountdummy, option];
            
            Return[dimlesscoef];
        ];
        
        
        
        
        
        camebackSum = 0;
        For[i = 1, i <= Length[term], i++,
            
            dummy = getterminfo[term[[i]], isdependenton, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, levelcountdummy, option];
            
            If[Length[dummy] > 1,
                camebackSum = 1;
                dummy = MakeSingleTerm[dummy, {}, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters];
            ];
            
            dimlesscoef[[1, 1]] = Join[dimlesscoef[[1, 1]], dummy[[1, 1]]];
            dimlesscoef[[1, 2]] = Join[dimlesscoef[[1, 2]], dummy[[1, 2]]];
            dimlesscoef[[1, 3]] = Join[dimlesscoef[[1, 3]], dummy[[1, 3]]];
            
        ];
        
        If[listProduct[dimlesscoef[[1, 1]]] == 0,
            dimlesscoef[[1, 1]] = {0};
            dimlesscoef[[1, 2]] = {Total[dimlesscoef[[1, 2]]]};
            dimlesscoef[[1, 3]] = {0};
            camebackSum = 0;
        ];
        
        If[camebackSum == 1,
            newterm = Expand[MakeSingleTerm[dimlesscoef, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters]];
            dimlesscoef = getterminfo[newterm[[1, 1, 1]], isdependenton, scalars, vectors, \
              tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, levelcountdummy, option];
        ];
        
        Return[dimlesscoef];
    ];


(*Checked*)
subsubcaseDot[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{{}}, {{}}, {{}}}}, i, ii, j, dummy, iii, irank, \
    camebackSum = 0, distributeddot, expandedterm, dotexpandedterm, chosen3 = 0, \
    dropindices = {}},
        
        For[i = 1, i <= Length[term], i++,
            expandedterm = Expand[term[[i]]];
            If[Head[expandedterm] == Plus,
                dotexpandedterm = 0;
                For[ii = 1, ii <= Length[expandedterm], ii++,
                    For[iii = 1, iii <= Length[term], iii++,
                        If[iii == 1,
                            If[iii == i,
                                distributeddot = expandedterm[[ii]];
                                ,
                                distributeddot = term[[iii]];
                            ];
                            ,
                            If[iii == i,
                                distributeddot = OperatorApplier[distributeddot, dotdenoters, \
                                  expandedterm[[ii]]];
                                ,
                                distributeddot = OperatorApplier[distributeddot, dotdenoters, \
                                  term[[iii]]];
                            ];
                        ];
                    ];
                    dotexpandedterm += distributeddot;
                ];
                
                dimlesscoef = getterminfo[dotexpandedterm, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
                
                Return[dimlesscoef];
            ];
        ];
        
        
        
        
        
        camebackSum = 0;
        For[i = 1, i <= Length[term], i++,
            
            dummy = getterminfo[term[[i]], isdependenton, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, levelcountdummy, option];
            
            If[Length[dummy] > 1,
                camebackSum = 1;
                dummy = MakeSingleTerm[dummy, {}, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters];
            ];
            
            dimlesscoef[[1, 1, 1]] = Join[dimlesscoef[[1, 1, 1]], dummy[[1, 1]]];
            dimlesscoef[[1, 2, 1]] = Join[dimlesscoef[[1, 2, 1]], dummy[[1, 2]]];
            dimlesscoef[[1, 3, 1]] = Join[dimlesscoef[[1, 3, 1]], dummy[[1, 3]]];
            
        ];
        
        If[camebackSum == 1,
            dimlesscoef = MakeSingleTerm[dimlesscoef, dotdenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            dimlesscoef = getterminfo[dimlesscoef[[1, 1, 1]], isdependenton, scalars, \
              vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
              dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ,
            dropindices = {};
            For[i = 1, i <= Length[dimlesscoef[[1, 2, 1]]], i++,
                chosen3 = 0;
                (*Zero Check*)
                If[dimlesscoef[[1, 1, 1, i]] == 0,
                    chosen3 = 1;
                    dimlesscoef[[1, 1]] = {0};
                    For[irank = 1, irank <= Length[dimlesscoef[[1, 2, 1]]], irank++,
                        If[irank == 1,
                            dummy = dimlesscoef[[1, 2, 1, irank]];
                            ,
                            dummy = dummy + dimlesscoef[[1, 2, 1, irank]] - 2;
                        ];
                    ];
                    dimlesscoef[[1, 2]] = {dummy};
                    dimlesscoef[[1, 3]] = {0};
                    Break[];
                ];
                
                If[dimlesscoef[[1, 2, 1, i]] == 0 && chosen3 == 0,
                    dropindices = Join[dropindices, {{i}}];
                    dimlesscoef[[1, 1]] = Join[dimlesscoef[[1, 1]], \
                      {dimlesscoef[[1, 1, 1, i]]}];
                    dimlesscoef[[1, 2]] = Join[dimlesscoef[[1, 2]], \
                      {dimlesscoef[[1, 2, 1, i]]}];
                    dimlesscoef[[1, 3]] = Join[dimlesscoef[[1, 3]], \
                      {dimlesscoef[[1, 3, 1, i]]}];
                ];
            ];
            
            If[Length[dropindices] > 0 && chosen3 == 0,
                dimlesscoef[[1, 1, 1]] = Delete[dimlesscoef[[1, 1, 1]], dropindices];
                dimlesscoef[[1, 2, 1]] = Delete[dimlesscoef[[1, 2, 1]], dropindices];
                dimlesscoef[[1, 3, 1]] = Delete[dimlesscoef[[1, 3, 1]], dropindices];
            ];
            
            dimlesscoef = subsubcaseDotSimpSimp[dimlesscoef, isdependenton, scalars, vectors, \
              tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, levelcountdummy, option];
            
        ];
        
        Return[dimlesscoef];
    ];


(*Checked*)
subsubcaseDotSimpSimp[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{chosen, iterms, newargumentparts2dummy, newargumentparts2 = {}},
        
        For[iterms = 1, iterms <= Length[term], iterms++,
            
            chosen = 0;
            
            If[Intersection[globalSimplificationMode, globalSimplificationDots] != {},
                {newargumentparts2dummy, chosen} = subsubcaseDotSimpSimpSimp[{term[[iterms]]}, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
                  slowIV, fastIV, levelcountdummy, option];
            ];
            
            If[chosen == 0,
                newargumentparts2dummy = MergeUnfoldedTerms[{term[[iterms]]}, dotdenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
                
                ,
                
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
                
                ,
                
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
            ];
            
        ];
        
        Return[newargumentparts2];
    ];


(*Checked*)
subsubcaseDotSimpSimpSimp[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{chosen = 0, newargumentparts2 = {}, leftofdoubledot, doubledot},
        
        If[Length[term[[1, 1]]] == 1 && chosen == 0,
            If[Head[term[[1, 1, 1]]] == List,
                If[Length[term[[1, 1, 1]]] == 3,
                    If[term[[1, 2, 1]] == {1, 2, 1},
                        If[term[[1, 3, 1]] == {1, 0, 1},
                            chosen = 1;
                            leftofdoubledot = OperatorApplier[term[[1, 1, 1, 3]], \
                              tensorproductdenoters, term[[1, 1, 1, 1]]];
                            doubledot = OperatorApplier[leftofdoubledot, \
                              doubledotdenoters, term[[1, 1, 1, 2]]];
                            newargumentparts2 = Join[newargumentparts2, {{{doubledot}, \
                              {0}, {1}}}];
                        ];
                    ];
                ];
            ];
        ];
        
        If[newargumentparts2 == {},
            newargumentparts2 = term;
        ];
        
        Return[{newargumentparts2, chosen}];
    ];


subsubcaseDotSimpSimpOLD[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{i, chosen = 0, iterms, newargumentparts2 = {}, flag1 = 0, flag2 = 0, scalar, \
    vector, termdotunfold, chosen2 = 0, chosen3 = 0, term2, termdotunfold2, leftofdoubledot, \
    doubledot, dummy},
        
        For[iterms = 1, iterms <= Length[term], iterms++,
            chosen = 0;
            
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == List,
                    If[Length[term[[iterms, 1, 1]]] == 3,
                        If[term[[iterms, 2, 1]] == {1, 2, 1},
                            If[term[[iterms, 3, 1]] == {1, 0, 1},
                                chosen = 1;
                                leftofdoubledot = OperatorApplier[term[[iterms, 1, 1, 3]], \
                                  tensorproductdenoters, term[[iterms, 1, 1, 1]]];
                                doubledot = OperatorApplier[leftofdoubledot, \
                                  doubledotdenoters, term[[iterms, 1, 1, 2]]];
                                newargumentparts2 = Join[newargumentparts2, {{{doubledot}, \
                                  {0}, {1}}}];
                            ];
                        ];
                    ];
                ];
            ];
            
            If[chosen == 0,
                dummy = MergeUnfoldedTerms[{term[[iterms]]}, dotdenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                newargumentparts2 = Join[newargumentparts2, dummy];
            ];
            
        ];
        
        Return[newargumentparts2];
    ];


(*Checked*)
subsubcaseTensorProduct[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{{}}, {{}}, {{}}}}, i, ii, j, dummy, iii, \
    camebackSum = 0, distributedtensor, expandedterm, tensorexpandedterm, \
    chosen3 = 0, dropindices = {}},
        
        For[i = 1, i <= Length[term], i++,
            expandedterm = Expand[term[[i]]];
            If[Head[expandedterm] == Plus,
                tensorexpandedterm = 0;
                For[ii = 1, ii <= Length[expandedterm], ii++,
                    For[iii = 1, iii <= Length[term], iii++,
                        If[iii == 1,
                            If[iii == i,
                                distributedtensor = expandedterm[[ii]];
                                ,
                                distributedtensor = term[[iii]];
                            ];
                            ,
                            If[iii == i,
                                distributedtensor = OperatorApplier[distributedtensor, \
                                  tensorproductdenoters, expandedterm[[ii]]];
                                ,
                                distributedtensor = OperatorApplier[distributedtensor, \
                                  tensorproductdenoters, term[[iii]]];
                            ];
                        ];
                    ];
                    tensorexpandedterm += distributedtensor;
                ];
                
                dimlesscoef = getterminfo[tensorexpandedterm, isdependenton, scalars, \
                  vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                Return[dimlesscoef];
            ];
        ];
        
        
        
        
        
        camebackSum = 0;
        For[i = 1, i <= Length[term], i++,
            
            dummy = getterminfo[term[[i]], isdependenton, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, levelcountdummy, option];
            
            If[Length[dummy] > 1,
                camebackSum = 1;
                dummy = MakeSingleTerm[dummy, {}, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters];
            ];
            
            dimlesscoef[[1, 1, 1]] = Join[dimlesscoef[[1, 1, 1]], dummy[[1, 1]]];
            dimlesscoef[[1, 2, 1]] = Join[dimlesscoef[[1, 2, 1]], dummy[[1, 2]]];
            dimlesscoef[[1, 3, 1]] = Join[dimlesscoef[[1, 3, 1]], dummy[[1, 3]]];
            
        ];
        
        If[camebackSum == 1,
            dimlesscoef = MakeSingleTerm[dimlesscoef, tensorproductdenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            dimlesscoef = getterminfo[dimlesscoef[[1, 1, 1]], isdependenton, scalars, \
              vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
              dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ,
            dropindices = {};
            For[i = 1, i <= Length[dimlesscoef[[1, 2, 1]]], i++,
                chosen3 = 0;
                (*Zero Check*)
                If[dimlesscoef[[1, 1, 1, i]] == 0,
                    chosen3 = 1;
                    dimlesscoef[[1, 1]] = {0};
                    dimlesscoef[[1, 2]] = {Total[dimlesscoef[[1, 2, 1]]]};
                    dimlesscoef[[1, 3]] = {0};
                    Break[];
                ];
                
                If[dimlesscoef[[1, 2, 1, i]] == 0 && chosen3 == 0,
                    dropindices = Join[dropindices, {{i}}];
                    dimlesscoef[[1, 1]] = Join[dimlesscoef[[1, 1]], \
                      {dimlesscoef[[1, 1, 1, i]]}];
                    dimlesscoef[[1, 2]] = Join[dimlesscoef[[1, 2]], \
                      {dimlesscoef[[1, 2, 1, i]]}];
                    dimlesscoef[[1, 3]] = Join[dimlesscoef[[1, 3]], \
                      {dimlesscoef[[1, 3, 1, i]]}];
                ];
            ];
            
            If[Length[dropindices] > 0 && chosen3 == 0,
                dimlesscoef[[1, 1, 1]] = Delete[dimlesscoef[[1, 1, 1]], dropindices];
                dimlesscoef[[1, 2, 1]] = Delete[dimlesscoef[[1, 2, 1]], dropindices];
                dimlesscoef[[1, 3, 1]] = Delete[dimlesscoef[[1, 3, 1]], dropindices];
            ];
            
            dimlesscoef = subsubcaseTensorProductSimpSimp[dimlesscoef, isdependenton, scalars, vectors, \
              tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, levelcountdummy, option];
            
        ];
        
        Return[dimlesscoef];
    ];


(*Checked*)
subsubcaseTensorProductSimpSimp[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{chosen = 0, iterms, newargumentparts2dummy, newargumentparts2 = {}},
        
        For[iterms = 1, iterms <= Length[term], iterms++,
            
            chosen = 0;
            
            If[Intersection[globalSimplificationMode, globalSimplificationTensorProducts] != {},
                {newargumentparts2dummy, chosen} = subsubcaseTensorProductSimpSimpSimp[{term[[iterms]]}, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
                  slowIV, fastIV, levelcountdummy, option];
            ];
            
            If[chosen == 0,
                newargumentparts2dummy = MergeUnfoldedTerms[{term[[iterms]]}, tensorproductdenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
                
                ,
                
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
                
                ,
                
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
            ];
            
        ];
        
        Return[newargumentparts2];
    ];


(*Checked*)
subsubcaseTensorProductSimpSimpSimp[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{chosen = 0, newargumentparts2 = {}},
        
        If[newargumentparts2 == {},
            newargumentparts2 = term;
        ];
        
        Return[{newargumentparts2, chosen}];
    ];


subsubcaseTensorProductSimpSimpOLD[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{chosen = 0, iterms, newargumentparts2 = {}, dummy},
        
        For[iterms = 1, iterms <= Length[term], iterms++,
            chosen = 0;
            
            If[chosen == 0,
                dummy = MergeUnfoldedTerms[{term[[iterms]]}, tensorproductdenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                newargumentparts2 = Join[newargumentparts2, dummy];
            ];
            
        ];
        
        Return[newargumentparts2];
    ];


(*Checked*)
subsubcaseDoubleDot[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{{}}, {{}}, {{}}}}, i, ii, j, dummy, iii, irank, \
    camebackSum = 0, distributeddoubledot, expandedterm, doubledotexpandedterm, \
    chosen3 = 0, dropindices},
        
        For[i = 1, i <= Length[term], i++,
            expandedterm = Expand[term[[i]]];
            If[Head[expandedterm] == Plus,
                doubledotexpandedterm = 0;
                For[ii = 1, ii <= Length[expandedterm], ii++,
                    For[iii = 1, iii <= Length[term], iii++,
                        If[iii == 1,
                            If[iii == i,
                                distributeddoubledot = expandedterm[[ii]];
                                ,
                                distributeddoubledot = term[[iii]];
                            ];
                            ,
                            If[iii == i,
                                distributeddoubledot = OperatorApplier[distributeddoubledot, \
                                  doubledotdenoters, expandedterm[[ii]]];
                                ,
                                distributeddoubledot = OperatorApplier[distributeddoubledot, \
                                  doubledotdenoters, term[[iii]]];
                            ];
                        ];
                    ];
                    doubledotexpandedterm += distributeddoubledot;
                ];
                
                dimlesscoef = getterminfo[doubledotexpandedterm, isdependenton, scalars, \
                  vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                Return[dimlesscoef];
            ];
        ];
        
        
        
        
        
        camebackSum = 0;
        For[i = 1, i <= Length[term], i++,
            
            dummy = getterminfo[term[[i]], isdependenton, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, levelcountdummy, option];
            
            If[Length[dummy] > 1,
                camebackSum = 1;
                dummy = MakeSingleTerm[dummy, {}, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters];
            ];
            
            dimlesscoef[[1, 1, 1]] = Join[dimlesscoef[[1, 1, 1]], dummy[[1, 1]]];
            dimlesscoef[[1, 2, 1]] = Join[dimlesscoef[[1, 2, 1]], dummy[[1, 2]]];
            dimlesscoef[[1, 3, 1]] = Join[dimlesscoef[[1, 3, 1]], dummy[[1, 3]]];
            
        ];
        
        If[camebackSum == 1,
            dimlesscoef = MakeSingleTerm[dimlesscoef, doubledotdenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            dimlesscoef = getterminfo[dimlesscoef[[1, 1, 1]], isdependenton, scalars, \
              vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
              dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ,
            dropindices = {};
            For[i = 1, i <= Length[dimlesscoef[[1, 2, 1]]], i++,
                chosen3 = 0;
                (*Zero Check*)
                If[dimlesscoef[[1, 1, 1, i]] == 0,
                    chosen3 = 1;
                    dimlesscoef[[1, 1]] = {0};
                    For[irank = 1, irank <= Length[dimlesscoef[[1, 2, 1]]], irank++,
                        If[irank == 1,
                            dummy = dimlesscoef[[1, 2, 1, irank]];
                            ,
                            dummy = dummy + dimlesscoef[[1, 2, 1, irank]] - 4;
                        ];
                    ];
                    dimlesscoef[[1, 2]] = {dummy};
                    dimlesscoef[[1, 3]] = {0};
                    Break[];
                ];
                
                If[dimlesscoef[[1, 2, 1, i]] == 0 && chosen3 == 0,
                    dropindices = Join[dropindices, {{i}}];
                    dimlesscoef[[1, 1]] = Join[dimlesscoef[[1, 1]], \
                      {dimlesscoef[[1, 1, 1, i]]}];
                    dimlesscoef[[1, 2]] = Join[dimlesscoef[[1, 2]], \
                      {dimlesscoef[[1, 2, 1, i]]}];
                    dimlesscoef[[1, 3]] = Join[dimlesscoef[[1, 3]], \
                      {dimlesscoef[[1, 3, 1, i]]}];
                ];
            ];
            
            If[Length[dropindices] > 0 && chosen3 == 0,
                dimlesscoef[[1, 1, 1]] = Delete[dimlesscoef[[1, 1, 1]], dropindices];
                dimlesscoef[[1, 2, 1]] = Delete[dimlesscoef[[1, 2, 1]], dropindices];
                dimlesscoef[[1, 3, 1]] = Delete[dimlesscoef[[1, 3, 1]], dropindices];
            ];
            
            dimlesscoef = subsubcaseDoubleDotSimpSimp[dimlesscoef, isdependenton, scalars, vectors, \
              tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, levelcountdummy, option];
            
        ];
        
        Return[dimlesscoef];
    ];


(*Checked*)
subsubcaseDoubleDotSimpSimp[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{chosen = 0, iterms, newargumentparts2dummy, newargumentparts2 = {}},
        
        For[iterms = 1, iterms <= Length[term], iterms++,
            
            chosen = 0;
            
            If[Intersection[globalSimplificationMode, globalSimplificationDoubleDots] != {},
                {newargumentparts2dummy, chosen} = subsubcaseDoubleDotSimpSimpSimp[{term[[iterms]]}, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
                  slowIV, fastIV, levelcountdummy, option];
            ];
            
            If[chosen == 0,
                newargumentparts2dummy = MergeUnfoldedTerms[{term[[iterms]]}, doubledotdenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
                
                ,
                
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
                
                ,
                
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
            ];
            
        ];
        
        Return[newargumentparts2];
    ];


(*Checked*)
subsubcaseDoubleDotSimpSimpSimp[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{chosen = 0, newargumentparts2 = {}},
        
        If[newargumentparts2 == {},
            newargumentparts2 = term;
        ];
        
        Return[{newargumentparts2, chosen}];
    ];


subsubcaseDoubleDotSimpSimpOLD[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{chosen = 0, iterms, newargumentparts2 = {}, dummy},
        
        For[iterms = 1, iterms <= Length[term], iterms++,
            chosen = 0;
            
            If[chosen == 0,
                dummy = MergeUnfoldedTerms[{term[[iterms]]}, doubledotdenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                newargumentparts2 = Join[newargumentparts2, dummy];
            ];
            
        ];
        
        Return[newargumentparts2];
    ];


(*Checked*)
subsubcasePlus[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, i, termparts, dropindices, \
    expandedterm = Expand[term]},
        
        If[Head[expandedterm] == Plus,
            For[i = 1, i <= Length[expandedterm], i++,
                termparts = getterminfo[expandedterm[[i]], isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
                
                dimlesscoef = Join[dimlesscoef, termparts];
            ];
            
            (*Zero Check*)
            dropindices = {};
            For[i = 1, i <= Length[dimlesscoef], i++,
                If[listProduct[dimlesscoef[[i, 1]]] == 0,
                    dropindices = Join[dropindices, {{i}}];
                ];
            ];
            If[Length[dropindices] > 0,
                dimlesscoef = Delete[dimlesscoef, dropindices];
            ];
            
            ,
            
            Print["CRITICAL ERROR: " <> functionName <> ": subsubcasePlus: Expanded sum became a non-Plus. Term is: ", term];
            Return[Null];
            
            ,
            
            Print["CRITICAL ERROR: " <> functionName <> ": subsubcasePlus: Expanded sum became a non-Plus. Term is: ", term];
            Return[Null];
        ];
        
        Return[dimlesscoef];
    ];


(*Checked*)
subsubcaseGradient[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, subscript = term[[2]], argument = term[[1]], \
    flag = 0, argumentparts, i, expandedargument, dimlesscoefparts, iii, bracketeddummy, \
    argumentpartsbracketed},
        
        expandedargument = Expand[argument];
        
        If[Head[expandedargument] == Plus,
            For[i = 1, i <= Length[expandedargument], i++,
                bracketeddummy = OperatorApplier[expandedargument[[i]], graddenoters, \
                  subscript];
                
                dimlesscoefparts = getterminfo[bracketeddummy, isdependenton, scalars, \
                  vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                dimlesscoef = Join[dimlesscoef, dimlesscoefparts];
            ];
            
            Return[dimlesscoef];
        ];
        
        
        
        
        
        (*Get information on the argument.*)
        argumentparts = getterminfo[argument, subscript, scalars, vectors, tensors, \
          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
          slowIV, fastIV, levelcountdummy, option];
        
        (*Analyze and simplify the information of each summed component individually.*)
        For[iii = 1, iii <= Length[argumentparts], iii++,
            argumentpartsbracketed = subsubcaseGradientSimp[{argumentparts[[iii]]}, \
              subscript, isdependenton, scalars, vectors, tensors, closurevar, \
              graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
              doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
            For[i = 1, i <= Length[argumentpartsbracketed], i++,
                If[listProduct[argumentpartsbracketed[[i, 1]]] == 0,
                    ,
                    dimlesscoef = Join[dimlesscoef, {argumentpartsbracketed[[i]]}];
                    ,
                    dimlesscoef = Join[dimlesscoef, {argumentpartsbracketed[[i]]}];
                ];
            ];
        ];
        
        (*Account for zeros.*)
        If[dimlesscoef == {},
            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            dimlesscoef = {{{0}, {argumentparts[[1, 2, 1]] + 1}, {0}}};
        ];
        
        Return[dimlesscoef];
    ];


(*Checked*)
subsubcaseGradientSimp[argumentparts_, subscript_, isdependenton_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{}, {}, {}}}, scalarmults = {{{}, {}, {}}}, \
    newargumentparts = {{{}, {}, {}}}, i, flag1 = 0, flag2 = 0, chosen = 0, \
    newargumentparts2, scalarmults2, dummyargumentparts, dummynewargumentparts, \
    count = 0, chosen2 = 0, dependentdotterm, iterms, imult, icombine},
        
        (*Take out the scalars that arent functions of the appropriate independent variable.*)
        For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
            If[argumentparts[[1, 2, i]] == 0 && argumentparts[[1, 3, i]] == 0,
                scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], {argumentparts[[1, 1, i]]}];
                scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], {argumentparts[[1, 2, i]]}];
                scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], {argumentparts[[1, 3, i]]}];
                ,
                newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                  {argumentparts[[1, 1, i]]}];
                newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                  {argumentparts[[1, 2, i]]}];
                newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                  {argumentparts[[1, 3, i]]}];
            ];
        ];
        
        (*Check to see if there is anything with the correct dependency in the gradient.*)
        If[Total[newargumentparts[[1, 3]]] == 0,
            dimlesscoef = {{{0}, {Total[argumentparts[[1, 2]]] + 1}, {0}}};
            Return[dimlesscoef];
        ];
        
        
        (*Decide the new dependency*)
        (*If the gradient independent variable is the same as 'isdependenton'.*)
        If[Intersection[subscript, isdependenton] != {},
            chosen = 1;
            
            If[Length[newargumentparts[[1, 1]]] > 0,
                newargumentparts = subsubcaseGradientSimpSimp[newargumentparts, \
                  subscript, isdependenton, scalars, vectors, tensors, closurevar, \
                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  levelcountdummy, option];
                ,
                dimlesscoef = {{{0}, {Total[argumentparts[[1, 2]]] + 1}, {0}}};
                Return[dimlesscoef];
            ];
            
            
            dimlesscoef = {};
            For[icombine = 1, icombine <= Length[newargumentparts], icombine++,
                For[imult = 1, imult <= Length[scalarmults], imult++,
                    dimlesscoef = Join[dimlesscoef, {{{}, {}, {}}}];
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      newargumentparts[[icombine, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      newargumentparts[[icombine, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      newargumentparts[[icombine, 3]]];
                    
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      scalarmults[[imult, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      scalarmults[[imult, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      scalarmults[[imult, 3]]];
                ];
            ];
            
        ];
        
        (*If the derivative independent variable is different than 'isdependenton'.*)
        If[chosen == 0,
            If[Length[scalarmults[[1, 1]]] > 0,
                scalarmults2 = getterminfo[listProduct[scalarmults[[1, 1]]], \
                  isdependenton, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  levelcountdummy, option];
                ,
                scalarmults2 = {{{}, {}, {}}};
            ];
            
            If[Length[newargumentparts[[1, 1]]] > 0,
                newargumentparts2 = getterminfo[listProduct[newargumentparts[[1, 1]]], \
                  isdependenton, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  levelcountdummy, option];
                
                newargumentparts2 = subsubcaseGradientSimpSimp[newargumentparts2, \
                  subscript, isdependenton, scalars, vectors, tensors, closurevar, \
                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  levelcountdummy, option];
                ,
                dimlesscoef = {{{0}, {Total[argumentparts[[1, 2]]] + 1}, {0}}};
                Return[dimlesscoef];
            ];
            
            
            dimlesscoef = {};
            For[icombine = 1, icombine <= Length[newargumentparts2], icombine++,
                For[imult = 1, imult <= Length[scalarmults2], imult++,
                    dimlesscoef = Join[dimlesscoef, {{{}, {}, {}}}];
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      newargumentparts2[[icombine, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      newargumentparts2[[icombine, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      newargumentparts2[[icombine, 3]]];
                    
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      scalarmults2[[imult, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      scalarmults2[[imult, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      scalarmults2[[imult, 3]]];
                ];
            ];
            
        ];
        
        Return[dimlesscoef];
    ];    


(*Checked*)
subsubcaseGradientSimpSimp[term_, subscript_, isdependenton_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{i, chosen = 0, iterms, newargumentparts2dummy, newargumentparts2 = {}},
        
        For[iterms = 1, iterms <= Length[term], iterms++,
            
            chosen = 0;
            
            If[Intersection[globalSimplificationMode, globalSimplificationGradients] != {},
                {newargumentparts2dummy, chosen} = subsubcaseGradientSimpSimpSimp[{term[[iterms]]}, subscript, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
                  slowIV, fastIV, levelcountdummy, option];
            ];
            
            If[chosen == 0,
                newargumentparts2 = Join[newargumentparts2, {term[[iterms]]}];
                
                (*Log the term*)
                newargumentparts2[[-1, 1]] = {OperatorApplier[listProduct[ \
                  newargumentparts2[[-1, 1]]], graddenoters, subscript]};
                
                (*Adjust tensor rank*)
                newargumentparts2[[-1, 2]] = {Total[newargumentparts2[[-1, 2]]] + 1};
                
                (*Assign Dependency*)
                If[Total[newargumentparts2[[-1, 3]]] >= 1,
                    newargumentparts2[[-1, 3]] = {1};
                    ,
                    newargumentparts2[[-1, 3]] = {0};
                ];
                
                ,
                
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
                
                ,
                
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
            ];
            
        ];
        
        Return[newargumentparts2];
    ];


(*Checked*)
subsubcaseGradientSimpSimpSimp[term_, subscript_, isdependenton_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{i, chosen = 0, newargumentparts2 = {}, flag1 = 0, flag2 = 0, scalar, \
    vector, termdotunfold, chosen2 = 0, chosen3 = 0, term2, termdotunfold2},
        
        If[Length[term[[1, 1]]] == 1 && chosen == 0,
            If[Head[term[[1, 1, 1]]] == Head[dotdenoters[[1]]],
                
                termdotunfold = UnfoldMergedTerms[term, dotdenoters, \
                  subscript, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                  fastIV, levelcountdummy, option];
                
                (*Zero Check*)
                chosen2 = 0;
                For[i = 1, i <= Length[termdotunfold[[1, 1, 1]]], i++,
                    If[termdotunfold[[1, 1, 1, i]] == 0,
                        chosen2 = 1;
                        newargumentparts2 = Join[newargumentparts2, {{{0}, {Total[term[[1, 2]]]}, {0}}}];
                        Break[];
                    ];
                ];
                
                If[chosen2 == 0,
                    termdotunfold2 = termdotunfold;
                    
                    If[Length[termdotunfold[[1, 1, 1]]] == 2,
                        chosen3 = 0;
                        If[termdotunfold[[1, 2, 1, 1]] == 1 && termdotunfold[[1, 2, 1, 2]] == 1,
                            chosen = 1;
                            chosen3 = 1;
                            
                            (*Take gradient of the first vector, if dependence allows for it.*)
                            If[termdotunfold[[1, 3, 1, 1]] == 1,
                                termdotunfold[[1, 1, 1, 1]] = OperatorApplier[ \
                                  termdotunfold[[1, 1, 1, 1]], graddenoters, subscript];
                                termdotunfold[[1, 2, 1, 1]] += 1;
                                
                                termdotunfold = MakeSingleTerm[termdotunfold, dotdenoters, \
                                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                                
                                term2 = getterminfo[termdotunfold[[1, 1, 1]], \
                                  isdependenton, scalars, vectors, tensors, closurevar, \
                                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                                  option];
                                
                                newargumentparts2 = Join[newargumentparts2, term2];
                            ];
                            
                            (*Take gradient of the second vector, if dependence allows for it.*)
                            If[termdotunfold2[[1, 3, 1, 2]] == 1,
                                termdotunfold2[[1, 1, 1, 2]] = OperatorApplier[ \
                                  termdotunfold2[[1, 1, 1, 2]], graddenoters, subscript];
                                termdotunfold2[[1, 2, 1, 2]] += 1;
                                
                                termdotunfold2[[1, 1, 1]] = Reverse[termdotunfold2[[1, 1, 1]]];
                                termdotunfold2[[1, 2, 1]] = Reverse[termdotunfold2[[1, 2, 1]]];
                                termdotunfold2[[1, 3, 1]] = Reverse[termdotunfold2[[1, 3, 1]]];
                                
                                termdotunfold2 = MakeSingleTerm[termdotunfold2, dotdenoters, \
                                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                                
                                term2 = getterminfo[termdotunfold2[[1, 1, 1]], \
                                  isdependenton, scalars, vectors, tensors, closurevar, \
                                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                                  option];
                                
                                newargumentparts2 = Join[newargumentparts2, term2];
                            ];
                        ];
                    ];
                ];
                
            ];
        ];
        
        
        If[Length[term[[1, 1]]] == 1 && chosen == 0,
            If[Head[term[[1, 1, 1]]] == Head[doubledotdenoters[[1]]],
                
                termdotunfold = UnfoldMergedTerms[term, doubledotdenoters, \
                  subscript, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                  fastIV, levelcountdummy, option];
                
                (*Zero Check*)
                chosen2 = 0;
                For[i = 1, i <= Length[termdotunfold[[1, 1, 1]]], i++,
                    If[termdotunfold[[1, 1, 1, i]] == 0,
                        chosen2 = 1;
                        newargumentparts2 = Join[newargumentparts2, {{{0}, {Total[term[[1, 2]]]}, {0}}}];
                        Break[];
                    ];
                ];
                
                If[chosen2 == 0,
                    termdotunfold2 = termdotunfold;
                    
                    If[Length[termdotunfold[[1, 1, 1]]] == 2,
                        chosen3 = 0;
                        If[termdotunfold[[1, 2, 1, 1]] == 2 && termdotunfold[[1, 2, 1, 2]] == 2,
                            chosen = 1;
                            chosen3 = 1;
                            
                            (*Take gradient of the first tensor, if dependence allows for it.*)
                            If[termdotunfold[[1, 3, 1, 1]] == 1,
                                termdotunfold[[1, 1, 1, 1]] = OperatorApplier[ \
                                  termdotunfold[[1, 1, 1, 1]], graddenoters, subscript];
                                termdotunfold[[1, 2, 1, 1]] += 1;
                                
                                termdotunfold = MakeSingleTerm[termdotunfold, doubledotdenoters, \
                                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                                  
                                term2 = getterminfo[termdotunfold[[1, 1, 1]], \
                                  isdependenton, scalars, vectors, tensors, closurevar, \
                                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                                   normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                                  option];
                                
                                newargumentparts2 = Join[newargumentparts2, term2];
                            ];
                            
                            (*Take gradient of the second tensor, if dependence allows for it.*)
                            If[termdotunfold2[[1, 3, 1, 2]] == 1,
                                termdotunfold2[[1, 1, 1, 2]] = OperatorApplier[ \
                                  termdotunfold2[[1, 1, 1, 2]], graddenoters, subscript];
                                termdotunfold2[[1, 2, 1, 2]] += 1;
                                
                                termdotunfold2[[1, 1, 1]] = Reverse[termdotunfold2[[1, 1, 1]]];
                                termdotunfold2[[1, 2, 1]] = Reverse[termdotunfold2[[1, 2, 1]]];
                                termdotunfold2[[1, 3, 1]] = Reverse[termdotunfold2[[1, 3, 1]]];
                                
                                termdotunfold2 = MakeSingleTerm[termdotunfold2, doubledotdenoters, \
                                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                                
                                term2 = getterminfo[termdotunfold2[[1, 1, 1]], \
                                  isdependenton, scalars, vectors, tensors, closurevar, \
                                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                                  option];
                                
                                newargumentparts2 = Join[newargumentparts2, term2];
                            ];
                        ];
                    ];
                ];
                
            ];
        ];
        
        If[newargumentparts2 == {},
            newargumentparts2 = term;
        ];
        
        Return[{newargumentparts2, chosen}];
    ];


subsubcaseGradientSimpSimpOLD[term_, subscript_, isdependenton_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{i, chosen = 0, iterms, newargumentparts2 = {}, flag1 = 0, flag2 = 0, scalar, \
    vector, termdotunfold, chosen2 = 0, chosen3 = 0, term2, termdotunfold2},
        
        For[iterms = 1, iterms <= Length[term], iterms++,
            chosen = 0;
            
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == Head[dotdenoters[[1]]],
                    
                    termdotunfold = UnfoldMergedTerms[{term[[iterms]]}, dotdenoters, \
                      subscript, scalars, vectors, tensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
                    (*Zero Check*)
                    chosen2 = 0;
                    For[i = 1, i <= Length[termdotunfold[[1, 1, 1]]], i++,
                        If[termdotunfold[[1, 1, 1, i]] == 0,
                            chosen2 = 1;
                            newargumentparts2 = Join[newargumentparts2, {{{0}, {Total[term[[iterms, 2]]]}, {0}}}];
                            Break[];
                        ];
                    ];
                    
                    If[chosen2 == 0,
                        termdotunfold2 = termdotunfold;
                        
                        If[Length[termdotunfold[[1, 1, 1]]] == 2,
                            chosen3 = 0;
                            If[termdotunfold[[1, 2, 1, 1]] == 1 && termdotunfold[[1, 2, 1, 2]] == 1,
                                chosen = 1;
                                chosen3 = 1;
                                
                                (*Take gradient of the first vector, if dependence allows for it.*)
                                If[termdotunfold[[1, 3, 1, 1]] == 1,
                                    termdotunfold[[1, 1, 1, 1]] = OperatorApplier[ \
                                      termdotunfold[[1, 1, 1, 1]], graddenoters, subscript];
                                    termdotunfold[[1, 2, 1, 1]] += 1;
                                    
                                    termdotunfold = MakeSingleTerm[termdotunfold, dotdenoters, \
                                      dotdenoters, tensorproductdenoters, doubledotdenoters];
                                    
                                    term2 = getterminfo[termdotunfold[[1, 1, 1]], \
                                      isdependenton, scalars, vectors, tensors, closurevar, \
                                      graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                                      normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                                      option];
                                    
                                    newargumentparts2 = Join[newargumentparts2, term2];
                                ];
                                
                                (*Take gradient of the second vector, if dependence allows for it.*)
                                If[termdotunfold2[[1, 3, 1, 2]] == 1,
                                    termdotunfold2[[1, 1, 1, 2]] = OperatorApplier[ \
                                      termdotunfold2[[1, 1, 1, 2]], graddenoters, subscript];
                                    termdotunfold2[[1, 2, 1, 2]] += 1;
                                    
                                    termdotunfold2[[1, 1, 1]] = Reverse[termdotunfold2[[1, 1, 1]]];
                                    termdotunfold2[[1, 2, 1]] = Reverse[termdotunfold2[[1, 2, 1]]];
                                    termdotunfold2[[1, 3, 1]] = Reverse[termdotunfold2[[1, 3, 1]]];
                                    
                                    termdotunfold2 = MakeSingleTerm[termdotunfold2, dotdenoters, \
                                      dotdenoters, tensorproductdenoters, doubledotdenoters];
                                    
                                    term2 = getterminfo[termdotunfold2[[1, 1, 1]], \
                                      isdependenton, scalars, vectors, tensors, closurevar, \
                                      graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                                      normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                                      option];
                                    
                                    newargumentparts2 = Join[newargumentparts2, term2];
                                ];
                            ];
                        ];
                    ];
                    
                ];
            ];
            
            
            (*If the argument of the gradient is a double dot product, where the gradient being
            applied can move onto one component in the double dot product.*)
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == Head[doubledotdenoters[[1]]],
                    
                    termdotunfold = UnfoldMergedTerms[{term[[iterms]]}, doubledotdenoters, \
                      subscript, scalars, vectors, tensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
                    (*Zero Check*)
                    chosen2 = 0;
                    For[i = 1, i <= Length[termdotunfold[[1, 1, 1]]], i++,
                        If[termdotunfold[[1, 1, 1, i]] == 0,
                            chosen2 = 1;
                            newargumentparts2 = Join[newargumentparts2, {{{0}, {Total[term[[iterms, 2]]]}, {0}}}];
                            Break[];
                        ];
                    ];
                    
                    If[chosen2 == 0,
                        termdotunfold2 = termdotunfold;
                        
                        If[Length[termdotunfold[[1, 1, 1]]] == 2,
                            chosen3 = 0;
                            If[termdotunfold[[1, 2, 1, 1]] == 2 && termdotunfold[[1, 2, 1, 2]] == 2,
                                chosen = 1;
                                chosen3 = 1;
                                
                                (*Take gradient of the first tensor, if dependence allows for it.*)
                                If[termdotunfold[[1, 3, 1, 1]] == 1,
                                    termdotunfold[[1, 1, 1, 1]] = OperatorApplier[ \
                                      termdotunfold[[1, 1, 1, 1]], graddenoters, subscript];
                                    termdotunfold[[1, 2, 1, 1]] += 1;
                                    
                                    termdotunfold = MakeSingleTerm[termdotunfold, doubledotdenoters, \
                                      dotdenoters, tensorproductdenoters, doubledotdenoters];
                                      
                                    term2 = getterminfo[termdotunfold[[1, 1, 1]], \
                                      isdependenton, scalars, vectors, tensors, closurevar, \
                                      graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                                      normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                                      option];
                                    
                                    newargumentparts2 = Join[newargumentparts2, term2];
                                ];
                                
                                (*Take gradient of the second tensor, if dependence allows for it.*)
                                If[termdotunfold2[[1, 3, 1, 2]] == 1,
                                    termdotunfold2[[1, 1, 1, 2]] = OperatorApplier[ \
                                      termdotunfold2[[1, 1, 1, 2]], graddenoters, subscript];
                                    termdotunfold2[[1, 2, 1, 2]] += 1;
                                    
                                    termdotunfold2[[1, 1, 1]] = Reverse[termdotunfold2[[1, 1, 1]]];
                                    termdotunfold2[[1, 2, 1]] = Reverse[termdotunfold2[[1, 2, 1]]];
                                    termdotunfold2[[1, 3, 1]] = Reverse[termdotunfold2[[1, 3, 1]]];
                                    
                                    termdotunfold2 = MakeSingleTerm[termdotunfold2, doubledotdenoters, \
                                      dotdenoters, tensorproductdenoters, doubledotdenoters];
                                    
                                    term2 = getterminfo[termdotunfold2[[1, 1, 1]], \
                                      isdependenton, scalars, vectors, tensors, closurevar, \
                                      graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                                      normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                                      option];
                                    
                                    newargumentparts2 = Join[newargumentparts2, term2];
                                ];
                            ];
                        ];
                    ];
                    
                ];
            ];
            
            
            If[chosen == 0,
                newargumentparts2 = Join[newargumentparts2, {term[[iterms]]}];
                
                (*Log the term*)
                newargumentparts2[[-1, 1]] = {OperatorApplier[listProduct[ \
                  newargumentparts2[[-1, 1]]], graddenoters, subscript]};
                
                (*Adjust tensor rank*)
                newargumentparts2[[-1, 2]] = {Total[newargumentparts2[[-1, 2]]] + 1};
                
                (*Assign Dependency*)
                If[Total[newargumentparts2[[-1, 3]]] >= 1,
                    newargumentparts2[[-1, 3]] = {1};
                    ,
                    newargumentparts2[[-1, 3]] = {0};
                ];
            ];
            
        ];
        
        Return[newargumentparts2];
    ];


(*Checked*)
subsubcaseDivergence[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, subscript = term[[2]], argument = term[[1]], \
    flag = 0, argumentparts, i, expandedargument, dimlesscoefparts, iii, bracketeddummy, \
    argumentpartsbracketed},
        
        expandedargument = Expand[argument];
        
        If[Head[expandedargument] == Plus,
            For[i = 1, i <= Length[expandedargument], i++,
                bracketeddummy = OperatorApplier[expandedargument[[i]], divdenoters, \
                  subscript];
                
                dimlesscoefparts = getterminfo[bracketeddummy, isdependenton, scalars, \
                  vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                dimlesscoef = Join[dimlesscoef, dimlesscoefparts];
            ];
            
            Return[dimlesscoef];
        ];
        
        
        
        
        
        (*Get information on the argument.*)
        argumentparts = getterminfo[argument, subscript, scalars, vectors, tensors, \
          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
          gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        
        (*Analyze and simplify the information of each summed component individually.*)
        For[iii = 1, iii <= Length[argumentparts], iii++,
            argumentpartsbracketed = subsubcaseDivergenceSimp[{argumentparts[[iii]]}, \
              subscript, isdependenton, scalars, vectors, tensors, closurevar, \
              graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            For[i = 1, i <= Length[argumentpartsbracketed], i++,
                If[listProduct[argumentpartsbracketed[[i, 1]]] == 0,
                    ,
                    dimlesscoef = Join[dimlesscoef, {argumentpartsbracketed[[i]]}];
                    ,
                    dimlesscoef = Join[dimlesscoef, {argumentpartsbracketed[[i]]}];
                ];
            ];
        ];
        
        (*Account for zeros.*)
        If[dimlesscoef == {},
            If[argumentparts == {{{0}, {0}, {0}}},
                Print["POTENTIAL CRITICAL ERROR: " <> functionName <> ": subsubcaseDivergence: The argument of a divergence came back from 'getterminfo' as {{{0},{0},{0}}}. Argument shouldn't be a scalar, however, procedure will continue. Term is: ", term];
                dimlesscoef = {{{0}, {0}, {0}}};
                ,
                argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters];
                dimlesscoef = {{{0}, {argumentparts[[1, 2, 1]] - 1}, {0}}};
                ,
                argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters];
                dimlesscoef = {{{0}, {argumentparts[[1, 2, 1]] - 1}, {0}}};
            ];
        ];
        
        Return[dimlesscoef];
    ];


(*Checked*)
subsubcaseDivergenceSimp[argumentparts_, subscript_, isdependenton_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{}, {}, {}}}, scalarmults = {{{}, {}, {}}}, \
    newargumentparts = {{{}, {}, {}}}, i, flag1 = 0, flag2 = 0, chosen = 0, \
    newargumentparts2, scalarmults2, dummyargumentparts, dummynewargumentparts, \
    count = 0, chosen2 = 0, dependentdotterm, iterms, imult, icombine},
        
        (*Take out the scalars that arent functions of the appropriate independent variable.*)
        For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
            If[argumentparts[[1, 2, i]] == 0 && argumentparts[[1, 3, i]] == 0,
                scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], {argumentparts[[1, 1, i]]}];
                scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], {argumentparts[[1, 2, i]]}];
                scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], {argumentparts[[1, 3, i]]}];
                ,
                newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                  {argumentparts[[1, 1, i]]}];
                newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                  {argumentparts[[1, 2, i]]}];
                newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                  {argumentparts[[1, 3, i]]}];
            ];
        ];
        
        (*Check that there is indeed a vector or tensor in the divergence*)
        If[Length[newargumentparts[[1, 1]]] > 0,
            If[Total[newargumentparts[[1, 2]]] == 0,
                Print["CRITICAL ERROR: " <> functionName <> ": subsubcaseDivergenceSimp: Divergence is being taken on a scalar. Term is: ", argumentparts];
                Return[Null];
            ];
            ,
            If[scalarmults == {{{0}, {0}, {0}}},
                Return[scalarmults];
            ];
            Print["CRITICAL ERROR: " <> functionName <> ": subsubcaseDivergenceSimp: Divergence was taken on scalar. Term is: ", argumentparts];
            Return[Null];
        ];
        
        (*Check to see if there is anything with the correct dependency in the divergence.*)
        If[Total[newargumentparts[[1, 3]]] == 0,
            dimlesscoef = {{{0}, {Total[argumentparts[[1, 2]]] - 1}, {0}}};
            Return[dimlesscoef];
        ];
        
        
        (*Decide the new dependency*)
        (*If the gradient independent variable is the same as 'isdependenton'.*)
        If[Intersection[subscript, isdependenton] != {},
            chosen = 1;
            
            If[Length[newargumentparts[[1, 1]]] > 0,
                newargumentparts = subsubcaseDivergenceSimpSimp[newargumentparts, \
                  subscript, isdependenton, scalars, vectors, tensors, closurevar, \
                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  levelcountdummy, option];
                ,
                Print["CRITICAL ERROR: " <> functionName <> ": subsubcaseDivergenceSimp: Divergence was taken on scalar. Term is: ", argumentparts];
                Return[Null];
            ];
            
            
            dimlesscoef = {};
            For[icombine = 1, icombine <= Length[newargumentparts], icombine++,
                For[imult = 1, imult <= Length[scalarmults], imult++,
                    dimlesscoef = Join[dimlesscoef, {{{}, {}, {}}}];
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      newargumentparts[[icombine, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      newargumentparts[[icombine, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      newargumentparts[[icombine, 3]]];
                    
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      scalarmults[[imult, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      scalarmults[[imult, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      scalarmults[[imult, 3]]];
                ];
            ];
            
        ];
        
        (*If the derivative independent variable is different than 'isdependenton'.*)
        If[chosen == 0,
            If[Length[scalarmults[[1, 1]]] > 0,
                scalarmults2 = getterminfo[listProduct[scalarmults[[1, 1]]], \
                  isdependenton, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  levelcountdummy, option];
                ,
                scalarmults2 = {{{}, {}, {}}};
            ];
            
            If[Length[newargumentparts[[1, 1]]] > 0,
                newargumentparts2 = getterminfo[listProduct[newargumentparts[[1, 1]]], \
                  isdependenton, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  levelcountdummy, option];
                
                newargumentparts2 = subsubcaseDivergenceSimpSimp[newargumentparts2, \
                  subscript, isdependenton, scalars, vectors, tensors, closurevar, \
                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
                ,
                Print["CRITICAL ERROR: " <> functionName <> ": subsubcaseDivergenceSimp: Divergence was taken on scalar. Term is: ", argumentparts];
                Return[Null];
            ];
            
            
            dimlesscoef = {};
            For[icombine = 1, icombine <= Length[newargumentparts2], icombine++,
                For[imult = 1, imult <= Length[scalarmults2], imult++,
                    dimlesscoef = Join[dimlesscoef, {{{}, {}, {}}}];
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      newargumentparts2[[icombine, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      newargumentparts2[[icombine, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      newargumentparts2[[icombine, 3]]];
                    
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      scalarmults2[[imult, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      scalarmults2[[imult, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      scalarmults2[[imult, 3]]];
                ];
            ];
            
        ];
        
        Return[dimlesscoef];
    ];


(*Checked*)
subsubcaseDivergenceSimpSimp[term_, subscript_, isdependenton_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{i, chosen, iterms, newargumentparts2dummy, newargumentparts2 = {}},
        
        For[iterms = 1, iterms <= Length[term], iterms++,
            
            chosen = 0;
            
            If[Intersection[globalSimplificationMode, globalSimplificationDivergences] != {},
                {newargumentparts2dummy, chosen} = subsubcaseDivergenceSimpSimpSimp[{term[[iterms]]}, subscript, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
                  slowIV, fastIV, levelcountdummy, option];
            ];
            
            If[chosen == 0,
                newargumentparts2 = Join[newargumentparts2, {term[[iterms]]}];
                
                (*Log the term*)
                newargumentparts2[[-1, 1]] = {OperatorApplier[listProduct[ \
                  newargumentparts2[[-1, 1]]], divdenoters, subscript]};
                
                (*Adjust tensor rank*)
                newargumentparts2[[-1, 2]] = {Total[newargumentparts2[[-1, 2]]] - 1};
                
                (*Assign Dependency*)
                If[Total[newargumentparts2[[-1, 3]]] >= 1,
                    newargumentparts2[[-1, 3]] = {1};
                    ,
                    newargumentparts2[[-1, 3]] = {0};
                ];
                
                ,
                
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
                
                ,
                
                newargumentparts2 = Join[newargumentparts2, newargumentparts2dummy];
            ];
            
        ];
        
        Return[newargumentparts2];
    ];


(*Checked*)
subsubcaseDivergenceSimpSimpSimp[term_, subscript_, isdependenton_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{i, chosen = 0, newargumentparts2 = {}, flag1 = 0, flag2 = 0, scalar, \
    scalardependency, vector, vectordependency, vectornabladependencytest, chosen3 = 0, \
    termdotunfold, termdotunfold2, term2, chosen2 = 0, vectornabladependencytestinfo, \
    vectordotunfold},
        
        (*If the argument of the divergence is a double dot product.*)
        If[Length[term[[1, 1]]] == 1 && chosen == 0,
            If[Head[term[[1, 1, 1]]] == Head[doubledotdenoters[[1]]],
                
                termdotunfold = UnfoldMergedTerms[term, doubledotdenoters, \
                  subscript, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                  fastIV, levelcountdummy, option];
                
                termdotunfold2 = termdotunfold;
                
                If[Length[termdotunfold[[1, 1, 1]]] == 2,
                    chosen3 = 0;
                    
                    If[termdotunfold[[1, 2, 1, 1]] == termdotunfold[[1, 2, 1, 2]] + 1,
                        chosen = 1;
                        chosen3 = 1;
                        
                        (*Distribute the divergence to the tensor n+1.*)
                        If[termdotunfold[[1, 3, 1, 1]] == 1,
                            termdotunfold[[1, 1, 1, 1]] = OperatorApplier[ \
                              termdotunfold[[1, 1, 1, 1]], divdenoters, subscript];
                            termdotunfold[[1, 2, 1, 1]] -= 1;
                            
                            termdotunfold = MakeSingleTerm[termdotunfold, doubledotdenoters, \
                              dotdenoters, tensorproductdenoters, doubledotdenoters];
                            term2 = getterminfo[termdotunfold[[1, 1, 1]], \
                              isdependenton, scalars, vectors, tensors, closurevar, \
                              graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                              normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                              option];
                            
                            newargumentparts2 = Join[newargumentparts2, term2];
                        ];
                        
                        (*Distribute the divergence (which turns to a gradient) to the
                        tensor n.*)
                        If[termdotunfold2[[1, 3, 1, 2]] == 1,
                            termdotunfold2[[1, 1, 1, 2]] = OperatorApplier[ \
                              termdotunfold2[[1, 1, 1, 2]], graddenoters, subscript];
                            termdotunfold2[[1, 2, 1, 2]] += 1;
                            termdotunfold2[[1, 1, 1, 1]] = OperatorApplier[ \
                              termdotunfold2[[1, 1, 1, 1]], pietrzykflipdenoters, \
                              pietrzykflipdenoters[[2]]];
                            
                            termdotunfold2 = MakeSingleTerm[termdotunfold2, \
                              doubledotdenoters, dotdenoters, tensorproductdenoters, \
                              doubledotdenoters];
                            term2 = getterminfo[termdotunfold2[[1, 1, 1]], \
                              isdependenton, scalars, vectors, tensors, closurevar, \
                              graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                              normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                              option];
                            
                            newargumentparts2 = Join[newargumentparts2, term2];
                        ];
                    ];
                ];
                
            ];
        ];
        
        
        (*If the argument of the divergence is a dot product.*)
        If[Length[term[[1, 1]]] == 1 && chosen == 0,
            If[Head[term[[1, 1, 1]]] == Head[dotdenoters[[1]]],
                
                termdotunfold = UnfoldMergedTerms[term, dotdenoters, \
                  subscript, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                  fastIV, levelcountdummy, option];
                
                (*Zero Check*)
                chosen2 = 0;
                For[i = 1, i <= Length[termdotunfold[[1, 1, 1]]], i++,
                    If[termdotunfold[[1, 1, 1, i]] == 0,
                        chosen2 = 1;
                        newargumentparts2 = Join[newargumentparts2, {{{0}, {Total[term[[1, 2]]]}, {0}}}];
                        Break[];
                    ];
                ];
                
                If[chosen2 == 0,
                    termdotunfold2 = termdotunfold;
                    
                    If[Length[termdotunfold[[1, 1, 1]]] == 2,
                        chosen3 = 0;
                        
                        If[termdotunfold[[1, 2, 1, 1]] == 2 && \
                          termdotunfold[[1, 2, 1, 2]] == 1,
                            chosen = 1;
                            chosen3 = 1;
                            
                            (*Distribute the divergence to the tensor.*)
                            If[termdotunfold[[1, 3, 1, 1]] == 1,
                                termdotunfold[[1, 1, 1, 1]] = OperatorApplier[ \
                                  termdotunfold[[1, 1, 1, 1]], divdenoters, subscript];
                                termdotunfold[[1, 2, 1, 1]] -= 1;
                                
                                termdotunfold = MakeSingleTerm[termdotunfold, dotdenoters, \
                                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                                term2 = getterminfo[termdotunfold[[1, 1, 1]], \
                                  isdependenton, scalars, vectors, tensors, closurevar, \
                                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                                  option];
                                
                                newargumentparts2 = Join[newargumentparts2, term2];
                            ];
                            
                            (*Distribute the divergence (which turns to a gradient) to the
                            vector.*)
                            If[termdotunfold2[[1, 3, 1, 2]] == 1,
                                termdotunfold2[[1, 1, 1, 2]] = OperatorApplier[ \
                                  termdotunfold2[[1, 1, 1, 2]], graddenoters, subscript];
                                termdotunfold2[[1, 2, 1, 2]] += 1;
                                termdotunfold2[[1, 1, 1, 1]] = OperatorApplier[ \
                                  termdotunfold2[[1, 1, 1, 1]], pietrzykflipdenoters, \
                                  pietrzykflipdenoters[[2]]];
                                
                                termdotunfold2 = MakeSingleTerm[termdotunfold2, \
                                  doubledotdenoters, dotdenoters, tensorproductdenoters, \
                                  doubledotdenoters];
                                term2 = getterminfo[termdotunfold2[[1, 1, 1]], \
                                  isdependenton, scalars, vectors, tensors, closurevar, \
                                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                                  option];
                                
                                newargumentparts2 = Join[newargumentparts2, term2];
                            ];
                        ];
                    ];
                ];
                
            ];
        ];
        
        
        If[Length[term[[1, 1]]] >= 2 && chosen == 0,
            If[Total[term[[1, 2]]] == 1,
                chosen = 1;
                
                scalar = {{{}, {}, {}}};
                vector = {{{}, {}, {}}};
                For[i = 1, i <= Length[term[[1, 2]]], i++,
                    If[term[[1, 2, i]] == 0,
                        scalar[[1, 1]] = Join[scalar[[1, 1]], {term[[1, 1, i]]}];
                        scalar[[1, 2]] = Join[scalar[[1, 2]], {term[[1, 2, i]]}];
                        scalar[[1, 3]] = Join[scalar[[1, 3]], {term[[1, 3, i]]}];
                        ,
                        vector[[1, 1]] = Join[vector[[1, 1]], {term[[1, 1, i]]}];
                        vector[[1, 2]] = Join[vector[[1, 2]], {term[[1, 2, i]]}];
                        vector[[1, 3]] = Join[vector[[1, 3]], {term[[1, 3, i]]}];
                    ];
                ];
                
                vectornabladependencytestinfo = getterminfo[vector[[1, 1, 1]], subscript, \
                  scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                  avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                vectornabladependencytest = MakeSingleTerm[vectornabladependencytestinfo, \
                  {}, dotdenoters, tensorproductdenoters, doubledotdenoters][[1, 3, 1]];
                vectordependency = vector[[1, 3, 1]];
                
                chosen2 = 0;
                
                (*Add the vector dotted with gradient scalar*)
                If[Head[vector[[1, 1, 1]]] == Head[dotdenoters[[1]]],
                    
                    vectordotunfold = UnfoldMergedTerms[vectornabladependencytestinfo, \
                      dotdenoters, subscript, scalars, vectors, tensors, closurevar, \
                      graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                      dotdenoters, tensorproductdenoters, doubledotdenoters, \
                      pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
                      slowIV, fastIV, levelcountdummy, option];
                    
                    If[Intersection[vectordotunfold[[1, 2, 1]], {1, 2}] == {1, 2},
                        If[vectordotunfold[[1, 2, 1, 1]] == 2,
                            chosen2 = 1;
                            newargumentparts2 = Join[newargumentparts2, {{{}, {}, {}}}];
                            newargumentparts2[[-1, 1]] = Join[ \
                              newargumentparts2[[-1, 1]], {OperatorApplier[ \
                              OperatorApplier[listProduct[scalar[[1, 1]]], graddenoters, \
                              subscript], dotdenoters, vector[[1, 1, 1]]]}];
                            newargumentparts2[[-1, 2]] = Join[ \
                              newargumentparts2[[-1, 2]], {0}];
                            If[Intersection[term[[1, 3]], {1}] != {},
                                newargumentparts2[[-1, 3]] = Join[ \
                                  newargumentparts2[[-1, 3]], {1}];
                                ,
                                newargumentparts2[[-1, 3]] = Join[ \
                                  newargumentparts2[[-1, 3]], {0}];
                            ];
                        ];
                    ];
                ];
                
                If[chosen2 == 0,
                    newargumentparts2 = Join[newargumentparts2, {{{}, {}, {}}}];
                    newargumentparts2[[-1, 1]] = Join[newargumentparts2[[-1, 1]], \
                      {OperatorApplier[vector[[1, 1, 1]], dotdenoters, OperatorApplier[ \
                      listProduct[scalar[[1, 1]]], graddenoters, subscript]]}];
                    newargumentparts2[[-1, 2]] = Join[newargumentparts2[[-1, 2]], {0}];
                    If[Intersection[term[[1, 3]], {1}] != {},
                        newargumentparts2[[-1, 3]] = Join[newargumentparts2[[-1, 3]], \
                          {1}];
                        ,
                        newargumentparts2[[-1, 3]] = Join[newargumentparts2[[-1, 3]], \
                          {0}];
                    ];
                ];
                
                (*Add the scalar times the divergence of vector*)
                If[vectornabladependencytest == 1,
                    newargumentparts2 = Join[newargumentparts2, {{{}, {}, {}}}];
                    For[i = 1, i <= Length[scalar[[1, 1]]], i++,
                        newargumentparts2[[-1, 1]] = Join[newargumentparts2[[-1, 1]], \
                          {scalar[[1, 1, i]]}];
                        newargumentparts2[[-1, 2]] = Join[newargumentparts2[[-1, 2]], \
                          {scalar[[1, 2, i]]}];
                        newargumentparts2[[-1, 3]] = Join[newargumentparts2[[-1, 3]], \
                          {scalar[[1, 3, i]]}];
                    ];
                    newargumentparts2[[-1, 1]] = Join[newargumentparts2[[-1, 1]], \
                      {OperatorApplier[vector[[1, 1, 1]], divdenoters, subscript]}];
                    newargumentparts2[[-1, 2]] = Join[newargumentparts2[[-1, 2]], {0}];
                    newargumentparts2[[-1, 3]] = Join[newargumentparts2[[-1, 3]], \
                      {vectordependency}];
                ];
                
            ];
        ];
        
        If[newargumentparts2 == {},
            newargumentparts2 = term;
        ];
        
        Return[{newargumentparts2, chosen}];
    ];


subsubcaseDivergenceSimpSimpOLD[term_, subscript_, isdependenton_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{i, chosen = 0, iterms, newargumentparts2 = {}, flag1 = 0, flag2 = 0, scalar, \
    scalardependency, vector, vectordependency, vectornabladependencytest, chosen3 = 0, \
    termdotunfold, termdotunfold2, term2, chosen2 = 0, vectornabladependencytestinfo, \
    vectordotunfold},
        
        For[iterms = 1, iterms <= Length[term], iterms++,
            chosen = 0;
            
            (*If the argument of the divergence is a double dot product.*)
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == Head[doubledotdenoters[[1]]],
                    
                    termdotunfold = UnfoldMergedTerms[{term[[iterms]]}, doubledotdenoters, \
                      subscript, scalars, vectors, tensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
                    termdotunfold2 = termdotunfold;
                    
                    (*If the argument of the divergence is a dot product between a consecutive
                    tensor ranked n+1 and tensor ranked n.*)
                    If[Length[termdotunfold[[1, 1, 1]]] == 2,
                        chosen3 = 0;
                        
                        If[termdotunfold[[1, 2, 1, 1]] == termdotunfold[[1, 2, 1, 2]] + 1,
                            chosen = 1;
                            chosen3 = 1;
                            
                            (*Distribute the divergence to the tensor n+1.*)
                            If[termdotunfold[[1, 3, 1, 1]] == 1,
                                termdotunfold[[1, 1, 1, 1]] = OperatorApplier[ \
                                  termdotunfold[[1, 1, 1, 1]], divdenoters, subscript];
                                termdotunfold[[1, 2, 1, 1]] -= 1;
                                
                                termdotunfold = MakeSingleTerm[termdotunfold, doubledotdenoters, \
                                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                                term2 = getterminfo[termdotunfold[[1, 1, 1]], \
                                  isdependenton, scalars, vectors, tensors, closurevar, \
                                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                                  option];
                                
                                newargumentparts2 = Join[newargumentparts2, term2];
                            ];
                            
                            (*Distribute the divergence (which turns to a gradient) to the
                            tensor n.*)
                            If[termdotunfold2[[1, 3, 1, 2]] == 1,
                                termdotunfold2[[1, 1, 1, 2]] = OperatorApplier[ \
                                  termdotunfold2[[1, 1, 1, 2]], graddenoters, subscript];
                                termdotunfold2[[1, 2, 1, 2]] += 1;
                                termdotunfold2[[1, 1, 1, 1]] = OperatorApplier[ \
                                  termdotunfold2[[1, 1, 1, 1]], pietrzykflipdenoters, \
                                  pietrzykflipdenoters[[2]]];
                                
                                termdotunfold2 = MakeSingleTerm[termdotunfold2, \
                                  doubledotdenoters, dotdenoters, tensorproductdenoters, \
                                  doubledotdenoters];
                                term2 = getterminfo[termdotunfold2[[1, 1, 1]], \
                                  isdependenton, scalars, vectors, tensors, closurevar, \
                                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                                  option];
                                
                                newargumentparts2 = Join[newargumentparts2, term2];
                            ];
                        ];
                    ];
                    
                ];
            ];
            
            
            (*If the argument of the divergence is a dot product.*)
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == Head[dotdenoters[[1]]],
                    
                    termdotunfold = UnfoldMergedTerms[{term[[iterms]]}, dotdenoters, \
                      subscript, scalars, vectors, tensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
                    (*Zero Check*)
                    chosen2 = 0;
                    For[i = 1, i <= Length[termdotunfold[[1, 1, 1]]], i++,
                        If[termdotunfold[[1, 1, 1, i]] == 0,
                            chosen2 = 1;
                            newargumentparts2 = Join[newargumentparts2, {{{0}, {Total[term[[iterms, 2]]]}, {0}}}];
                            Break[];
                        ];
                    ];
                    
                    If[chosen2 == 0,
                        termdotunfold2 = termdotunfold;
                        
                        (*If the argument of the divergence is a dot product between a consecutive
                        tensor and vector.*)
                        If[Length[termdotunfold[[1, 1, 1]]] == 2,
                            chosen3 = 0;
                            
                            If[termdotunfold[[1, 2, 1, 1]] == 2 && \
                              termdotunfold[[1, 2, 1, 2]] == 1,
                                chosen = 1;
                                chosen3 = 1;
                                
                                (*Distribute the divergence to the tensor.*)
                                If[termdotunfold[[1, 3, 1, 1]] == 1,
                                    termdotunfold[[1, 1, 1, 1]] = OperatorApplier[ \
                                      termdotunfold[[1, 1, 1, 1]], divdenoters, subscript];
                                    termdotunfold[[1, 2, 1, 1]] -= 1;
                                    
                                    termdotunfold = MakeSingleTerm[termdotunfold, dotdenoters, \
                                      dotdenoters, tensorproductdenoters, doubledotdenoters];
                                    term2 = getterminfo[termdotunfold[[1, 1, 1]], \
                                      isdependenton, scalars, vectors, tensors, closurevar, \
                                      graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                                      normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                                      option];
                                    
                                    newargumentparts2 = Join[newargumentparts2, term2];
                                ];
                                
                                (*Distribute the divergence (which turns to a gradient) to the
                                vector.*)
                                If[termdotunfold2[[1, 3, 1, 2]] == 1,
                                    termdotunfold2[[1, 1, 1, 2]] = OperatorApplier[ \
                                      termdotunfold2[[1, 1, 1, 2]], graddenoters, subscript];
                                    termdotunfold2[[1, 2, 1, 2]] += 1;
                                    termdotunfold2[[1, 1, 1, 1]] = OperatorApplier[ \
                                      termdotunfold2[[1, 1, 1, 1]], pietrzykflipdenoters, \
                                      pietrzykflipdenoters[[2]]];
                                    
                                    termdotunfold2 = MakeSingleTerm[termdotunfold2, \
                                      doubledotdenoters, dotdenoters, tensorproductdenoters, \
                                      doubledotdenoters];
                                    term2 = getterminfo[termdotunfold2[[1, 1, 1]], \
                                      isdependenton, scalars, vectors, tensors, closurevar, \
                                      graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                                      normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                                      option];
                                    
                                    newargumentparts2 = Join[newargumentparts2, term2];
                                ];
                            ];
                        ];
                    ];
                    
                ];
            ];
            
            
            (*If argument is scalars times a vector, simplify it by distributing the
            divergence operator.*)
            If[Length[term[[iterms, 1]]] >= 2 && chosen == 0,
                If[Total[term[[iterms, 2]]] == 1,
                    chosen = 1;
                    
                    scalar = {{{}, {}, {}}};
                    vector = {{{}, {}, {}}};
                    For[i = 1, i <= Length[term[[iterms, 2]]], i++,
                        If[term[[iterms, 2, i]] == 0,
                            scalar[[1, 1]] = Join[scalar[[1, 1]], {term[[iterms, 1, i]]}];
                            scalar[[1, 2]] = Join[scalar[[1, 2]], {term[[iterms, 2, i]]}];
                            scalar[[1, 3]] = Join[scalar[[1, 3]], {term[[iterms, 3, i]]}];
                            ,
                            vector[[1, 1]] = Join[vector[[1, 1]], {term[[iterms, 1, i]]}];
                            vector[[1, 2]] = Join[vector[[1, 2]], {term[[iterms, 2, i]]}];
                            vector[[1, 3]] = Join[vector[[1, 3]], {term[[iterms, 3, i]]}];
                        ];
                    ];
                    
                    vectornabladependencytestinfo = getterminfo[vector[[1, 1, 1]], subscript, \
                      scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                      avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                      normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                    vectornabladependencytest = MakeSingleTerm[vectornabladependencytestinfo, \
                      {}, dotdenoters, tensorproductdenoters, doubledotdenoters][[1, 3, 1]];
                    vectordependency = vector[[1, 3, 1]];
                    
                    chosen2 = 0;
                    
                    (*Add the vector dotted with gradient scalar*)
                    If[Head[vector[[1, 1, 1]]] == Head[dotdenoters[[1]]],
                        
                        vectordotunfold = UnfoldMergedTerms[vectornabladependencytestinfo, \
                          dotdenoters, subscript, scalars, vectors, tensors, closurevar, \
                          graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                          dotdenoters, tensorproductdenoters, doubledotdenoters, \
                          pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
                          slowIV, fastIV, levelcountdummy, option];
                        
                        If[Intersection[vectordotunfold[[1, 2, 1]], {1, 2}] == {1, 2},
                            If[vectordotunfold[[1, 2, 1, 1]] == 2,
                                chosen2 = 1;
                                newargumentparts2 = Join[newargumentparts2, {{{}, {}, {}}}];
                                newargumentparts2[[-1, 1]] = Join[ \
                                  newargumentparts2[[-1, 1]], {OperatorApplier[ \
                                  OperatorApplier[listProduct[scalar[[1, 1]]], graddenoters, \
                                  subscript], dotdenoters, vector[[1, 1, 1]]]}];
                                newargumentparts2[[-1, 2]] = Join[ \
                                  newargumentparts2[[-1, 2]], {0}];
                                If[Intersection[term[[iterms, 3]], {1}] != {},
                                    newargumentparts2[[-1, 3]] = Join[ \
                                      newargumentparts2[[-1, 3]], {1}];
                                    ,
                                    newargumentparts2[[-1, 3]] = Join[ \
                                      newargumentparts2[[-1, 3]], {0}];
                                ];
                            ];
                        ];
                    ];
                    
                    If[chosen2 == 0,
                        newargumentparts2 = Join[newargumentparts2, {{{}, {}, {}}}];
                        newargumentparts2[[-1, 1]] = Join[newargumentparts2[[-1, 1]], \
                          {OperatorApplier[vector[[1, 1, 1]], dotdenoters, OperatorApplier[ \
                          listProduct[scalar[[1, 1]]], graddenoters, subscript]]}];
                        newargumentparts2[[-1, 2]] = Join[newargumentparts2[[-1, 2]], {0}];
                        If[Intersection[term[[iterms, 3]], {1}] != {},
                            newargumentparts2[[-1, 3]] = Join[newargumentparts2[[-1, 3]], \
                              {1}];
                            ,
                            newargumentparts2[[-1, 3]] = Join[newargumentparts2[[-1, 3]], \
                              {0}];
                        ];
                    ];
                    
                    (*Add the scalar times the divergence of vector*)
                    If[vectornabladependencytest == 1,
                        newargumentparts2 = Join[newargumentparts2, {{{}, {}, {}}}];
                        For[i = 1, i <= Length[scalar[[1, 1]]], i++,
                            newargumentparts2[[-1, 1]] = Join[newargumentparts2[[-1, 1]], \
                              {scalar[[1, 1, i]]}];
                            newargumentparts2[[-1, 2]] = Join[newargumentparts2[[-1, 2]], \
                              {scalar[[1, 2, i]]}];
                            newargumentparts2[[-1, 3]] = Join[newargumentparts2[[-1, 3]], \
                              {scalar[[1, 3, i]]}];
                        ];
                        newargumentparts2[[-1, 1]] = Join[newargumentparts2[[-1, 1]], \
                          {OperatorApplier[vector[[1, 1, 1]], divdenoters, subscript]}];
                        newargumentparts2[[-1, 2]] = Join[newargumentparts2[[-1, 2]], {0}];
                        newargumentparts2[[-1, 3]] = Join[newargumentparts2[[-1, 3]], \
                          {vectordependency}];
                    ];
                    
                ];
            ];
            
            
            If[chosen == 0,
                newargumentparts2 = Join[newargumentparts2, {term[[iterms]]}];
                
                (*Log the term*)
                newargumentparts2[[-1, 1]] = {OperatorApplier[listProduct[ \
                  newargumentparts2[[-1, 1]]], divdenoters, subscript]};
                
                (*Adjust tensor rank*)
                newargumentparts2[[-1, 2]] = {Total[newargumentparts2[[-1, 2]]] - 1};
                
                (*Assign Dependency*)
                If[Total[newargumentparts2[[-1, 3]]] >= 1,
                    newargumentparts2[[-1, 3]] = {1};
                    ,
                    newargumentparts2[[-1, 3]] = {0};
                ];
            ];
        ];
        
        Return[newargumentparts2];
    ];


(*Checked*)
subsubcaseAverage[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, subscript = term[[2]], argument = term[[1, 1]], flag = 0, \
    argumentparts, i, expandedargument, dimlesscoefparts, iii, bracketeddummy, \
    argumentpartsbracketed, IVofsubscript, avgdenotersIV = avgdenoters[[2, ;;, 2]], \
    avgdenoterssymbols = avgdenoters[[2, ;;, 1]], \
    avgdenoterssplitdomainsymbols = avgdenoters[[2, ;;, 3]], iavg},
        
        For[iavg = 1, iavg <= Length[avgdenoterssymbols], iavg++,
            If[Intersection[{subscript}, avgdenoterssymbols[[iavg]]] != {} || \
              Intersection[{subscript}, Flatten[avgdenoterssplitdomainsymbols[[iavg]]]] != {},
                
                expandedargument = Expand[argument];
                
                If[Head[expandedargument] == Plus,
                    For[i = 1, i <= Length[expandedargument], i++,
                        bracketeddummy = OperatorApplier[expandedargument[[i]], \
                          avgdenoters[[1]], subscript];
                        
                        dimlesscoefparts = getterminfo[bracketeddummy, isdependenton, \
                          scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                          avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                          divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                          levelcountdummy, option];
                        
                        dimlesscoef = Join[dimlesscoef, dimlesscoefparts];
                    ];
                    
                    Return[dimlesscoef];
                ];
                
                
                
                
                
                argumentparts = getterminfo[argument, avgdenoters[[2, iavg, 2]], scalars, \
                  vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                For[iii = 1, iii <= Length[argumentparts], iii++,
                    argumentpartsbracketed = subsubcaseAverageSimp[{argumentparts[[iii]]}, \
                      subscript, avgdenoters[[2, iavg, 2]], isdependenton, scalars, vectors, \
                      tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                      dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                      gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                      option];
                    For[i = 1, i <= Length[argumentpartsbracketed], i++,
                        If[listProduct[argumentpartsbracketed[[i, 1]]] == 0,
                            ,
                            dimlesscoef = Join[dimlesscoef, {argumentpartsbracketed[[i]]}];
                            ,
                            dimlesscoef = Join[dimlesscoef, {argumentpartsbracketed[[i]]}];
                        ];
                    ];
                ];
                If[dimlesscoef == {},
                    argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters];
                    dimlesscoef = {{{0}, {argumentparts[[1, 2, 1]]}, {0}}};
                ];
                
                Return[dimlesscoef];
            ];
        ];
        
        Print["CRITICAL ERROR: " <> functionName <> ": subsubcaseAverage: Unidentified average. Term is: ", term];
        Return[Null];
    ];


(*Checked*)
subsubcaseAverageSimp[argumentparts_, subscript_, IVofsubscript_, isdependenton_, scalars_, \
vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{}, {}, {}}}, scalarmults = {{{}, {}, {}}}, \
    newargumentparts=  {{{}, {}, {}}}, i, flag1 = 0, flag2 = 0, chosen = 0, \
    newargumentparts2, scalarmults2, dummyargumentparts, dummynewargumentparts, \
    chosen2 = 0, iterms, imult, icombine},
        
        (*Take out the scalars that arent functions of the appropriate independent variable.*)
        For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
            If[argumentparts[[1, 3, i]] == 0,
                scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], {argumentparts[[1, 1, i]]}];
                scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], {argumentparts[[1, 2, i]]}];
                scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], {argumentparts[[1, 3, i]]}];
                ,
                newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                  {argumentparts[[1, 1, i]]}];
                newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                  {argumentparts[[1, 2, i]]}];
                newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                  {argumentparts[[1, 3, i]]}];
            ];
        ];
        
        
        (*Decide the new dependency*)
        (*If the gradient independent variable is the same as 'isdependenton'.*)
        If[Intersection[IVofsubscript, isdependenton] != {},
            chosen = 1;
            
            If[Length[newargumentparts[[1, 1]]] > 0,
                newargumentparts = subsubcaseAverageSimpSimp[newargumentparts, subscript, \
                  IVofsubscript, isdependenton, scalars, vectors, tensors, closurevar, \
                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  levelcountdummy, option];
            ];
            
            
            dimlesscoef = {};
            For[icombine = 1, icombine <= Length[newargumentparts], icombine++,
                For[imult = 1, imult <= Length[scalarmults], imult++,
                    dimlesscoef = Join[dimlesscoef, {{{}, {}, {}}}];
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      newargumentparts[[icombine, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      newargumentparts[[icombine, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      newargumentparts[[icombine, 3]]];
                    
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      scalarmults[[imult, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      scalarmults[[imult, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      scalarmults[[imult, 3]]];
                ];
            ];
            
        ];
        
        (*If the derivative independent variable is different than 'isdependenton'.*)
        If[chosen == 0,
            If[Length[scalarmults[[1, 1]]] > 0,
                scalarmults2 = getterminfo[listProduct[scalarmults[[1, 1]]], isdependenton, \
                  scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                  avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                ,
                scalarmults2 = {{{}, {}, {}}};
            ];
            
            If[Length[newargumentparts[[1, 1]]] > 0,
                newargumentparts2 = getterminfo[listProduct[newargumentparts[[1, 1]]], \
                  isdependenton, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  levelcountdummy, option];
                
                newargumentparts2 = subsubcaseAverageSimpSimp[newargumentparts2, subscript, \
                  IVofsubscript, isdependenton, scalars, vectors, tensors, closurevar, \
                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  levelcountdummy, option];
                ,
                newargumentparts2 = {{{}, {}, {}}};
            ];
            
            
            dimlesscoef = {};
            For[icombine = 1, icombine <= Length[newargumentparts2], icombine++,
                For[imult = 1, imult <= Length[scalarmults2], imult++,
                    dimlesscoef = Join[dimlesscoef, {{{}, {}, {}}}];
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      newargumentparts2[[icombine, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      newargumentparts2[[icombine, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      newargumentparts2[[icombine, 3]]];
                    
                    dimlesscoef[[-1, 1]] = Join[dimlesscoef[[-1, 1]], \
                      scalarmults2[[imult, 1]]];
                    dimlesscoef[[-1, 2]] = Join[dimlesscoef[[-1, 2]], \
                      scalarmults2[[imult, 2]]];
                    dimlesscoef[[-1, 3]] = Join[dimlesscoef[[-1, 3]], \
                      scalarmults2[[imult, 3]]];
                ];
            ];
            
        ];
        
        Return[dimlesscoef];
    ];


subsubcaseAverageSimpSimp[term_, subscript_, IVofsubscript_, isdependenton_, scalars_, \
vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{i, chosen = 0, iterms, newargumentparts2 = {}, flag1 = 0, flag2 = 0, scalar, \
    vector, termdotunfold, chosen2 = 0, izerocheck, divsym, gradsym, avgargument, \
    newargumentpartsdependency, iclosure, chosen3 = 0, inavg = 0, outavg = 0, success = 1, \
    dependentIndices = {}, newterminfo, newavgargument, irank, dummy, infloopstop, flagdot, \
    termnotdot, termdot, newtermnotdot, flagnotdot},
        
        For[iterms = 1, iterms <= Length[term], iterms++,
            chosen = 0;
            
            (*If there is a zero in the term.*)
            For[izerocheck = 1, izerocheck <= Length[term[[iterms, 1]]], izerocheck++,
                If[term[[iterms, 1, izerocheck]] == 0 && chosen == 0,
                    chosen = 1;
                    
                    newargumentparts2 = Join[newargumentparts2, {term[[iterms]]}];
                
                    (*Log the term*)
                    newargumentparts2[[-1, 1]] = {0};
                    
                    (*Adjust scalar, vector, or tensor indication*)
                    newargumentparts2[[-1, 2]] = {Total[newargumentparts2[[-1, 2]]]};
                    
                    (*Assign Dependency*)
                    newargumentparts2[[-1, 3]] = {0};
                    
                    Break[];
                ];
            ];
            
            
            (*If argument of the average is a closure variable, which is 0.*)
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                newterminfo = getterminfo[term[[iterms, 1, 1]], IVofsubscript, scalars, \
                  vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, \
                  derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
                  pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                  fastIV, levelcountdummy, option];
                newterminfo = MakeSingleTerm[newterminfo, {}, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters];
                If[newterminfo[[1, 3, 1]] == 1 && Length[newterminfo[[1, 1]]] == 1,
                    For[iclosure = 1, iclosure <= Length[closurevar], iclosure++,
                        If[newterminfo[[1, 1, 1]] == closurevar[[iclosure]],
                            If[Intersection[avgdenoters[[2, ;;, 1, 1]], {subscript}] != {},
                                chosen = 1;
                                newargumentparts2 = Join[newargumentparts2, {{{0}, \
                                  {term[[iterms, 2, 1]]}, {0}}}];
                                Break[];
                            ];
                        ];
                        If[chosen == 1,
                            Break[];
                        ];
                    ];
                ];
            ];
            
            
            (*If the argument of the average is a dot product*)
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == Head[dotdenoters[[1]]],
                    
                    termdotunfold = UnfoldMergedTerms[{term[[iterms]]}, dotdenoters, \
                      IVofsubscript, scalars, vectors, tensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
                    inavg = 0;
                    outavg = 0;
                    success = 1;
                    dependentIndices = {};
                    For[i = 1, i <= Length[termdotunfold[[1, 3, 1]]], i++,
                        If[termdotunfold[[1, 3, 1, i]] == 1 && inavg == 1 && outavg == 0,
                            dependentIndices = Join[dependentIndices, {i}];
                        ];
                        If[termdotunfold[[1, 3, 1, i]] == 1 && inavg == 0,
                            If[outavg == 0,
                                inavg = 1;
                                dependentIndices = Join[dependentIndices, {i}];
                                ,
                                success = 0;
                                Break[];
                            ];
                        ];
                        If[termdotunfold[[1, 3, 1, i]] == 0 && inavg == 1,
                            inavg = 0;
                            outavg = 1;
                        ];
                    ];
                    
                    If[success == 1,
                        chosen3 = 0;
                        
                        (*The average being applied can move onto one component in the dot
                        product.*)
                        If[Length[dependentIndices] == 1 && chosen3 == 0,
                            
                            If[Head[termdotunfold[[1, 1, 1, dependentIndices[[1]]]]] == \
                              Head[graddenoters[[1]]],
                                If[Intersection[termdotunfold[[1, 1, 1, \
                                  dependentIndices[[1]], 2]], IVofsubscript] == {},
                                    chosen = 1;
                                    chosen3 = 1;
                                    
                                    termdotunfold[[1, 1, 1, dependentIndices[[1]], 1]] = \
                                      OperatorApplier[termdotunfold[[1, 1, 1, \
                                      dependentIndices[[1]], 1]], avgdenoters[[1]], \
                                      subscript];
                                    termdotunfold[[1, 3, 1, dependentIndices[[1]]]] = 0;
                                    
                                    termdotunfold = MakeSingleTerm[termdotunfold, \
                                      dotdenoters, dotdenoters, tensorproductdenoters, \
                                      doubledotdenoters];
                                    newterminfo = getterminfo[termdotunfold, \
                                      isdependenton, scalars, vectors, tensors, \
                                      closurevar, graddenoters, divdenoters, avgdenoters, \
                                      derivativedenoters, dotdenoters, \
                                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                                      gradform, normalvecform, Uform, slowIV, fastIV, \
                                      levelcountdummy, option];
                                    newterminfo = MakeSingleTerm[newterminfo, {}, \
                                      dotdenoters, tensorproductdenoters, \
                                      doubledotdenoters];
                                    
                                    newargumentparts2 = Join[newargumentparts2, \
                                      newterminfo];
                                ];
                            ];
                            
                        ];
                        
                        infloopstop = 0;
                        For[i = 1, i <= Length[termdotunfold[[1, 3, 1]]], i++,
                            If[termdotunfold[[1, 3, 1, i]] == 0,
                                infloopstop = 1;
                            ];
                        ];
                        If[chosen3 == 0 && infloopstop == 1,
                            chosen = 1;
                            chosen3 = 1;
                            (*Create term from the consectively dotted terms.*)
                            For[i = 1, i <= Length[dependentIndices], i++,
                                If[i == 1,
                                    newavgargument = termdotunfold[[1, 1, 1, \
                                      dependentIndices[[i]]]];
                                    ,
                                    newavgargument = OperatorApplier[newavgargument, \
                                      dotdenoters, termdotunfold[[1, 1, 1, \
                                      dependentIndices[[i]]]]];
                                ];
                            ];
                            termdotunfold[[1, 1, 1, dependentIndices[[1]]]] = \
                              OperatorApplier[newavgargument, avgdenoters[[1]], \
                              subscript];
                            
                            (*Find the rank of the new term.*)
                            For[irank = 1, irank <= Length[dependentIndices], irank++,
                                If[irank == 1,
                                    dummy = termdotunfold[[1, 2, 1, \
                                      dependentIndices[[irank]]]];
                                    ,
                                    dummy = Abs[dummy - termdotunfold[[1, 2, 1, \
                                      dependentIndices[[irank]]]]];
                                ];
                            ];
                            termdotunfold[[1, 2, 1, dependentIndices[[1]]]] = dummy;
                            
                            (*Assign dependency*)
                            termdotunfold[[1, 3, 1, dependentIndices[[1]]]] = 0;
                            
                            (*Drop the components that were used to make the term.*)
                            If[Length[dependentIndices] > 1,
                                For[i = 2, i <= Length[dependentIndices], i++,
                                    termdotunfold[[1, 1, 1]] = Drop[ \
                                      termdotunfold[[1, 1, 1]], {dependentIndices[[i]]}];
                                    termdotunfold[[1, 2, 1]] = Drop[ \
                                      termdotunfold[[1, 2, 1]], {dependentIndices[[i]]}];
                                    termdotunfold[[1, 3, 1]] = Drop[ \
                                      termdotunfold[[1, 3, 1]], {dependentIndices[[i]]}];
                                ];
                            ];
                            
                            termdotunfold = MakeSingleTerm[termdotunfold, dotdenoters, \
                              dotdenoters, tensorproductdenoters, doubledotdenoters];
                            newterminfo = getterminfo[termdotunfold[[1, 1, 1]], \
                              isdependenton, scalars, vectors, tensors, closurevar, \
                              graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                              dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                              divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                              levelcountdummy, option];
                            newterminfo = MakeSingleTerm[newterminfo, {}, dotdenoters, \
                              tensorproductdenoters, doubledotdenoters];
                            
                            newargumentparts2 = Join[newargumentparts2, newterminfo];
                        
                        ];
                    ];
                    
                ];
            ];
            
            
            If[Length[term[[iterms, 1]]] == 2 && chosen == 0,
                
                flagdot = 0;
                flagnotdot = 0;
                For[i = 1, i <= Length[term[[iterms, 1]]], i++,
                    If[Head[term[[iterms, 1, i]]] == Head[dotdenoters[[1]]],
                        If[flagdot == 1,
                            flagdot = 0;
                            Break[];
                            ,
                            flagdot = 1;
                        ];
                        termdot = {{{term[[iterms, 1, i]]}, {term[[iterms, 2, i]]}, {term[[iterms, 3, i]]}}};
                        ,
                        termnotdot = {{{term[[iterms, 1, i]]}, {term[[iterms, 2, i]]}, {term[[iterms, 3, i]]}}};
                        flagnotdot = 1;
                        ,
                        termnotdot = {{{term[[iterms, 1, i]]}, {term[[iterms, 2, i]]}, {term[[iterms, 3, i]]}}};
                        flagnotdot = 1;
                    ];
                ];
                
                If[flagnotdot == 1,
                    newtermnotdot = getterminfo[termnotdot[[1, 1, 1]], IVofsubscript, \
                      scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                      avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                      normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                    If[Length[newtermnotdot] == 1 && Length[newtermnotdot[[1, 1]]] == 1 && \
                      newtermnotdot[[1, 3, 1]] == 1 && newtermnotdot[[1, 2, 1]] == 0,
                        flagnotdot = 1;
                        ,
                        flagnotdot = 0;
                        ,
                        flagnotdot = 0;
                    ];
                ];
                
                If[flagdot == 1 && flagnotdot == 1,
                    
                    termdotunfold = UnfoldMergedTerms[termdot, dotdenoters, \
                      IVofsubscript, scalars, vectors, tensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                      gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                    
                    inavg = 0;
                    outavg = 0;
                    success = 1;
                    dependentIndices = {};
                    For[i = 1, i <= Length[termdotunfold[[1, 3, 1]]], i++,
                        If[termdotunfold[[1, 3, 1, i]] == 1 && inavg == 1 && outavg == 0,
                            dependentIndices = Join[dependentIndices, {i}];
                        ];
                        If[termdotunfold[[1, 3, 1, i]] == 1 && inavg == 0,
                            If[outavg == 0,
                                inavg = 1;
                                dependentIndices = Join[dependentIndices, {i}];
                                ,
                                success = 0;
                                Break[];
                            ];
                        ];
                        If[termdotunfold[[1, 3, 1, i]] == 0 && inavg == 1,
                            inavg = 0;
                            outavg = 1;
                        ];
                    ];
                    
                    If[success == 1,
                        chosen3 = 0;
                        
                        infloopstop = 0;
                        For[i = 1, i <= Length[termdotunfold[[1, 3, 1]]], i++,
                            If[termdotunfold[[1, 3, 1, i]] == 0,
                                infloopstop = 1;
                            ];
                        ];
                        If[chosen3 == 0 && infloopstop == 1,
                            chosen = 1;
                            chosen3 = 1;
                            (*Create term from the consectively dotted terms.*)
                            For[i = 1, i <= Length[dependentIndices], i++,
                                If[i == 1,
                                    newavgargument = termdotunfold[[1, 1, 1, \
                                      dependentIndices[[i]]]];
                                    ,
                                    newavgargument = OperatorApplier[newavgargument, \
                                      dotdenoters, termdotunfold[[1, 1, 1, \
                                      dependentIndices[[i]]]]];
                                ];
                            ];
                            termdotunfold[[1, 1, 1, dependentIndices[[1]]]] = \
                              OperatorApplier[newavgargument * newtermnotdot[[1, 1, 1]], \
                              avgdenoters[[1]], subscript];
                            
                            (*Find the rank of the new term.*)
                            For[irank = 1, irank <= Length[dependentIndices], irank++,
                                If[irank == 1,
                                    dummy = termdotunfold[[1, 2, 1, \
                                      dependentIndices[[irank]]]];
                                    ,
                                    dummy = Abs[dummy - termdotunfold[[1, 2, 1, \
                                      dependentIndices[[irank]]]]];
                                ];
                            ];
                            termdotunfold[[1, 2, 1, dependentIndices[[1]]]] = dummy;
                            
                            (*Assign dependency*)
                            termdotunfold[[1, 3, 1, dependentIndices[[1]]]] = 0;
                            
                            (*Drop the components that were used to make the term.*)
                            If[Length[dependentIndices] > 1,
                                For[i = 2, i <= Length[dependentIndices], i++,
                                    termdotunfold[[1, 1, 1]] = Drop[ \
                                      termdotunfold[[1, 1, 1]], {dependentIndices[[i]]}];
                                    termdotunfold[[1, 2, 1]] = Drop[ \
                                      termdotunfold[[1, 2, 1]], {dependentIndices[[i]]}];
                                    termdotunfold[[1, 3, 1]] = Drop[ \
                                      termdotunfold[[1, 3, 1]], {dependentIndices[[i]]}];
                                ];
                            ];
                            
                            termdotunfold = MakeSingleTerm[termdotunfold, dotdenoters, \
                              dotdenoters, tensorproductdenoters, doubledotdenoters];
                            newterminfo = getterminfo[termdotunfold[[1, 1, 1]], \
                              isdependenton, scalars, vectors, tensors, closurevar, \
                              graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                              dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                              divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                              levelcountdummy, option];
                            newterminfo = MakeSingleTerm[newterminfo, {}, dotdenoters, \
                              tensorproductdenoters, doubledotdenoters];
                            
                            newargumentparts2 = Join[newargumentparts2, newterminfo];
                        
                        ];
                    ];
                    
                ];
            ];
            
            
            (*If the argument of the average is a double dot product, where the average being
            applied can move onto one component in the doulbe dot product.*)
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == Head[doubledotdenoters[[1]]],
                    
                    termdotunfold = UnfoldMergedTerms[{term[[iterms]]}, doubledotdenoters, \
                      IVofsubscript, scalars, vectors, tensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
                    If[Total[termdotunfold[[1, 3, 1]]] == 1,
                        chosen = 1;
                        For[i = 1, i <= Length[termdotunfold[[1, 3, 1]]], i++,
                            If[termdotunfold[[1, 3, 1, i]] == 1,
                                chosen2 = 0;
                                
                                If[chosen2 == 0,
                                    termdotunfold[[1, 1, 1, i]] = OperatorApplier[ \
                                      termdotunfold[[1, 1, 1, i]], avgdenoters[[1]], \
                                      subscript];
                                    
                                    termdotunfold = MakeSingleTerm[termdotunfold, doubledotdenoters, \
                                      dotdenoters, tensorproductdenoters, doubledotdenoters];
                                    newterminfo = getterminfo[termdotunfold[[1, 1, 1]], \
                                      isdependenton, scalars, vectors, tensors, closurevar, \
                                      graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                                      dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                                      divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                                      levelcountdummy, option];
                                    newterminfo = MakeSingleTerm[newterminfo, {}, dotdenoters, \
                                      tensorproductdenoters, doubledotdenoters];
                                    
                                    newargumentparts2 = Join[newargumentparts2, newterminfo];
                                ];
                                
                                Break[];
                            ];
                        ]; 
                    ];
                    
                ];
            ];
            
            
            (*If argument of the average is a divergence, where the average being applied
            can move passed the divergence and onto the corresponding argument.*)
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == Head[divdenoters[[1]]],
                    divsym = term[[iterms, 1, 1, 2]];
                    If[Intersection[divsym, IVofsubscript] == {},
                        chosen = 1;
                        newargumentparts2 = Join[newargumentparts2, {term[[iterms]]}];
                        
                        (*Log the term*)
                        avgargument = OperatorApplier[term[[iterms, 1, 1, 1]], \
                          avgdenoters[[1]], subscript];
                        newargumentparts2[[-1, 1]] = {OperatorApplier[avgargument, \
                          divdenoters, divsym]};
                        
                        (*Adjust scalar, vector, or tensor indication*)
                        newargumentparts2[[-1, 2]] = {term[[iterms, 2, 1]]};
                        
                        (*Assign Dependency*)
                        If[Intersection[IVofsubscript, isdependenton] != {},
                            newargumentparts2[[-1, 3]] = {0};
                            ,
                            newargumentparts2[[-1, 3]] = {term[[iterms, 3, 1]]};
                        ];
                    ];
                ];
            ];
            
            
            (*If argument of the average is a gradient, where the average being applied
            can move passed the gradient and onto the corresponding argument.*)
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == Head[graddenoters[[1]]],
                    gradsym = term[[iterms, 1, 1, 2]];
                    If[Intersection[gradsym, IVofsubscript] == {},
                        chosen = 1;
                        newargumentparts2 = Join[newargumentparts2, {term[[iterms]]}];
                        
                        (*Log the term*)
                        avgargument = OperatorApplier[term[[iterms, 1, 1, 1]], \
                          avgdenoters[[1]], subscript];
                        newargumentparts2[[-1, 1]] = {OperatorApplier[avgargument, \
                          graddenoters, gradsym]};
                        
                        (*Adjust scalar, vector, or tensor indication*)
                        newargumentparts2[[-1, 2]] = {term[[iterms, 2, 1]]};
                        
                        (*Assign Dependency*)
                        If[Intersection[IVofsubscript, isdependenton] != {},
                            newargumentparts2[[-1, 3]] = {0};
                            ,
                            newargumentparts2[[-1, 3]] = {term[[iterms, 3, 1]]};
                        ];
                    ];
                ];
            ];
            
            
            If[chosen == 0,
                newargumentparts2 = Join[newargumentparts2, {term[[iterms]]}];
                
                (*Log the term*)
                newargumentparts2[[-1, 1]] = {OperatorApplier[listProduct[ \
                  newargumentparts2[[-1, 1]]], avgdenoters[[1]], subscript]};
                
                (*Adjust scalar, vector, or tensor indication*)
                newargumentparts2[[-1, 2]] = {Total[newargumentparts2[[-1, 2]]]};
                
                (*Assign Dependency*)
                If[Intersection[IVofsubscript, isdependenton] != {},
                    newargumentparts2[[-1, 3]] = {0};
                    ,
                    If[Total[newargumentparts2[[-1, 3]]] >= 1,
                        newargumentparts2[[-1, 3]] = {1};
                        ,
                        newargumentparts2[[-1, 3]] = {0};
                    ];
                ];
            ];
            
        ];
        
        Return[newargumentparts2];
    ];


(*Checked*)
MergeProducts[argumentparts_, bracketform_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_] :=
    Block[{dimlesscoef = argumentparts, iii, j},
        
        dimlesscoef = MergeUnfoldedTerms[dimlesscoef, bracketform, dotdenoters, \
          tensorproductdenoters, doubledotdenoters];
        
        dimlesscoef = CorrectTensorProductTerms[dimlesscoef, tensorproductdenoters];
        
        For[iii = 1, iii <= Length[dimlesscoef], iii++,
            For[j = 1, j <= Length[dimlesscoef[[iii, 1]]], j++,
                If[dimlesscoef[[iii, 1, j]] == 0,
                    If[dimlesscoef[[iii, 2, j]] < 0,
                        dimlesscoef[[iii, 2, j]] = 0;
                        Print[functionName <> ": MergeProducts: Careful; the piece of code that inserts a rank of 0 for a variable of 0 was used. You should check the output of this function and make sure the tensor orders make sense."];
                    ];
                ];
            ];
        ];
        
        Return[dimlesscoef];
    ];


(*Checked*)
MergeUnfoldedTerms[argumentparts_, bracketform_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_] :=
    Block[{i, iii, flag, dimlesscoef = argumentparts, chosen, dummy, iterm, flag1, \
    izerocheck, irank, chosen2 = 0},
        
        For[iii = 1, iii <= Length[dimlesscoef], iii++,
            For[i = 1, i <= Length[dimlesscoef[[iii, 1]]], i++,
                If[Head[dimlesscoef[[iii, 1, i]]] == List,
                    
                    chosen = 0;
                    
                    (*Check for zeroes in the product.*)
                    For[izerocheck = 1, izerocheck <= Length[dimlesscoef[[iii, 1, i]]], \
                      izerocheck++,
                        If[dimlesscoef[[iii, 1, i, izerocheck]] == 0,
                            chosen = 1;
                            
                            (*Implement tensor multiplication (with the zero)*)
                            dimlesscoef[[iii, 1, i]] = 0;
                            
                            (*Adjust tensor rank based on the 'bracketform'*)
                            chosen2 = 0;
                            If[Head[bracketform[[1]]] == Head[dotdenoters[[1]]] && \
                              chosen2 == 0,
                                chosen2 = 1;
                                For[irank = 1, irank <= Length[dimlesscoef[[iii, 2, i]]], \
                                  irank++,
                                    If[dimlesscoef[[iii, 2, i, irank]] == 0,
                                        Print["CRITICAL ERROR: " <> functionName <> ": MergeUnfoldedTerms: Found a scalar in a dot product operator. Term is: ", argumentparts];
                                        Return[Null];
                                    ];
                                    If[irank == 1,
                                        dummy = dimlesscoef[[iii, 2, i, irank]];
                                        ,
                                        dummy = dummy + dimlesscoef[[iii, 2, i, irank]] - 2;
                                    ];
                                ];
                                dimlesscoef[[iii, 2, i]] = dummy;
                            ];
                            If[Head[bracketform[[1]]] == Head[doubledotdenoters[[1]]] && \
                              chosen2 == 0,
                                chosen2 = 1;
                                For[irank = 1, irank <= Length[dimlesscoef[[iii, 2, i]]], \
                                  irank++,
                                    If[dimlesscoef[[iii, 2, i, irank]] == 0 || dimlesscoef[[iii, 2, i, irank]] == 1,
                                        Print["CRITICAL ERROR: " <> functionName <> ": MergeUnfoldedTerms: Found a scalar or vector in a double-dot product operator. Term is: ", argumentparts];
                                        Return[Null];
                                    ];
                                    If[irank == 1,
                                        dummy = dimlesscoef[[iii, 2, i, irank]];
                                        ,
                                        dummy = dummy + dimlesscoef[[iii, 2, i, irank]] - 4;
                                    ];
                                ];
                                dimlesscoef[[iii, 2, i]] = dummy;
                            ];
                            If[Head[bracketform[[1]]] == Head[tensorproductdenoters[[1]]] && \
                              chosen2 == 0,
                                chosen2 = 1;
                                For[irank = 1, irank <= Length[dimlesscoef[[iii, 2, i]]], \
                                  irank++,
                                    If[dimlesscoef[[iii, 2, i, irank]] == 0,
                                        Print["CRITICAL ERROR: " <> functionName <> ": MergeUnfoldedTerms: Found a scalar in a tensor product operator. Term is: ", argumentparts];
                                        Return[Null];
                                    ];
                                ];
                                dimlesscoef[[iii, 2, i]] = Total[dimlesscoef[[iii, 2, i]]];
                            ];
                            If[chosen2 == 0,
                                Print["CRITICAL ERROR: " <> functionName <> ": MergeUnfoldedTerms: Unidentified product operator. Term is: ", argumentparts];
                                Return[Null];
                            ];
                            
                            (*Adjust dependency*)
                            dimlesscoef[[iii, 3, i]] = 0;
                            
                            Break[];
                        ];
                    ];
                    
                    
                    If[chosen == 0,
                        
                        (* Implement tensor multiplication*)
                        For[iterm = 1, iterm <= Length[dimlesscoef[[iii, 1, i]]], iterm++,
                            If[iterm == 1,
                                dummy = dimlesscoef[[iii, 1, i, iterm]];
                                ,
                                dummy = OperatorApplier[dummy, bracketform, \
                                  dimlesscoef[[iii, 1, i, iterm]]];
                            ];
                        ];
                        dimlesscoef[[iii, 1, i]] = dummy;
                        
                        (*Adjust tensor rank based on the 'bracketform'*)
                        chosen2 = 0;
                        If[Head[bracketform[[1]]] == Head[dotdenoters[[1]]] && \
                          chosen2 == 0,
                            chosen2 = 1;
                            For[irank = 1, irank <= Length[dimlesscoef[[iii, 2, i]]], \
                              irank++,
                                If[dimlesscoef[[iii, 2, i, irank]] == 0,
                                    Print["CRITICAL ERROR: " <> functionName <> ": MergeUnfoldedTerms: Found a scalar in a dot product operator. Term is: ", argumentparts];
                                    Return[Null];
                                ];
                                If[irank == 1,
                                    dummy = dimlesscoef[[iii, 2, i, irank]];
                                    ,
                                    dummy = dummy + dimlesscoef[[iii, 2, i, irank]] - 2;
                                ];
                            ];
                            dimlesscoef[[iii, 2, i]] = dummy;
                        ];
                        If[Head[bracketform[[1]]] == Head[doubledotdenoters[[1]]] && \
                          chosen2 == 0,
                            chosen2 = 1;
                            For[irank = 1, irank <= Length[dimlesscoef[[iii, 2, i]]], \
                              irank++,
                                If[dimlesscoef[[iii, 2, i, irank]] == 0 || dimlesscoef[[iii, 2, i, irank]] == 1,
                                    Print["CRITICAL ERROR: " <> functionName <> ": MergeUnfoldedTerms: Found a scalar or vector in a double-dot product operator. Term is: ", argumentparts];
                                    Return[Null];
                                ];
                                If[irank == 1,
                                    dummy = dimlesscoef[[iii, 2, i, irank]];
                                    ,
                                    dummy = dummy + dimlesscoef[[iii, 2, i, irank]] - 4;
                                ];
                            ];
                            dimlesscoef[[iii, 2, i]] = dummy;
                        ];
                        If[Head[bracketform[[1]]] == Head[tensorproductdenoters[[1]]] && \
                          chosen2 == 0,
                            chosen2 = 1;
                            For[irank = 1, irank <= Length[dimlesscoef[[iii, 2, i]]], \
                              irank++,
                                If[dimlesscoef[[iii, 2, i, irank]] == 0,
                                    Print["CRITICAL ERROR: " <> functionName <> ": MergeUnfoldedTerms: Found a scalar in a tensor product operator. Term is: ", argumentparts];
                                    Return[Null];
                                ];
                            ];
                            dimlesscoef[[iii, 2, i]] = Total[dimlesscoef[[iii, 2, i]]];
                        ];
                        If[chosen2 == 0,
                            Print["CRITICAL ERROR: " <> functionName <> ": MergeUnfoldedTerms: Unidentified product operator. Term is: ", argumentparts];
                            Return[Null];
                        ];
                        
                        (*Adjust dependency*)
                        If[Total[dimlesscoef[[iii, 3, i]]] >= 1,
                            dimlesscoef[[iii, 3, i]] = 1;
                            ,
                            dimlesscoef[[iii, 3, i]] = 0;
                        ];
                    ];
                    
                ];
            ];
        ];
        
        Return[dimlesscoef];
    ];


CorrectTensorProductTerms[argumentparts_, tensorproductdenoters_] :=
    Block[{i, ii, iii, dimlesscoef = argumentparts, tensorproductterms, tensororder, \
    tensordependency, totaltensorproduct, nottensorproductterms, nottensororder, \
    nottensordependency, tensorproductlocation, totaltensororder, totaltensordependency},
        
        For[iii = 1, iii <= Length[dimlesscoef], iii++,
            
            tensorproductterms = {};
            tensororder = {};
            tensordependency = {};
            nottensorproductterms = {};
            nottensororder = {};
            nottensordependency = {};
            
            For[i = 1, i <= Length[dimlesscoef[[iii, 2]]], i++,
                If[dimlesscoef[[iii, 2, i]] > 0,
                    tensorproductterms = Join[tensorproductterms, {dimlesscoef[[iii, 1, i]]}];
                    tensororder = Join[tensororder, {dimlesscoef[[iii, 2, i]]}];
                    tensordependency = Join[tensordependency, {dimlesscoef[[iii, 3, i]]}];
                    If[Length[tensorproductterms] == 1,
                        tensorproductlocation = i;
                    ];
                    ,
                    nottensorproductterms = Join[tensorproductterms, \
                      {dimlesscoef[[iii, 1, i]]}];
                    nottensororder = Join[tensororder, {dimlesscoef[[iii, 2, i]]}];
                    nottensordependency = Join[tensordependency, {dimlesscoef[[iii, 3, i]]}];
                ];
            ];
            
            If[Length[tensororder] > 1,
            
                (*Create tensor product*)
                For[ii = 1, ii <= Length[tensorproductterms], ii++,
                    If[ii == 1,
                        totaltensorproduct = tensorproductterms[[ii]];
                        ,
                        totaltensorproduct = OperatorApplier[totaltensorproduct, \
                          tensorproductdenoters, tensorproductterms[[ii]]];
                    ];
                ];
                
                (*Adjust scalar, vector, or tensor indication*)
                totaltensororder = Total[tensororder];
                 
                (*Adjust dependency*)
                If[Total[tensordependency] > 0,
                    totaltensordependency = 1;
                    ,
                    totaltensordependency = 0;
                ];
                
                (*Create output vector*)
                nottensorproductterms = Insert[nottensorproductterms, totaltensorproduct, \
                  tensorproductlocation];
                nottensororder = Insert[nottensororder, totaltensororder, \
                  tensorproductlocation];
                nottensordependency = Insert[nottensordependency, totaltensordependency, \
                  tensorproductlocation];
                
                dimlesscoef[[iii, 1]] = nottensorproductterms;
                dimlesscoef[[iii, 2]] = nottensororder;
                dimlesscoef[[iii, 3]] = nottensordependency;
                
            ];
        
        ];
        
        Return[dimlesscoef];
    ];


UnfoldMergedTerms[term_, bracketform_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{termhandle = term, i, ii, iii, argumentparts, Nterms},
        
        For[i = 1, i <= Length[term], i++,
            For[ii = 1, ii <= Length[term[[i, 1]]], ii++,
                If[Head[term[[i, 1, ii]]] == Head[bracketform[[1]]],
                    
                    termhandle[[i, 1, ii]] = {};
                    termhandle[[i, 2, ii]] = {};
                    termhandle[[i, 3, ii]] = {};
                    
                    Nterms = Length[term[[i, 1, ii]]];
                    For[iii = 1, iii <= Nterms, iii++,
                        
                        argumentparts = getterminfo[term[[i, 1, ii, iii]], isdependenton, \
                          scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                          avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                          divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                          levelcountdummy, option];
                        
                        argumentparts = MakeSingleTerm[argumentparts, bracketform, \
                          dotdenoters, tensorproductdenoters, doubledotdenoters];
                        
                        (*Record the term*)
                        termhandle[[i, 1, ii]] = Join[termhandle[[i, 1, ii]], \
                          argumentparts[[1, 1]]];
                        
                        (*Record the tensor rank*)
                        termhandle[[i, 2, ii]] = Join[termhandle[[i, 2, ii]], \
                          argumentparts[[1, 2]]];
                        
                        (*Adjust dependency*)
                        termhandle[[i, 3, ii]] = Join[termhandle[[i, 3, ii]], \
                          argumentparts[[1, 3]]];
                        
                    ];
                    
                ];
            ];
        ];
        
        Return[termhandle];
    ];


(*Checked*)
MakeSingleTerm[argumentparts_, bracketform_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_] :=
    Block[{iii, flag, dimlesscoef = argumentparts, dimlesstotal, dimlessrank, i, izero, \
    dimlessdependency, newdimlesscoef},
        
        If[bracketform == {},
            ,
            dimlesscoef = MergeProducts[dimlesscoef, bracketform, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            ,
            dimlesscoef = MergeProducts[dimlesscoef, bracketform, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
        ];
        
        
        (*Multiply things out*)
        For[iii = 1, iii <= Length[dimlesscoef], iii++,
            dimlesscoef[[iii, 1]] = {listProduct[dimlesscoef[[iii, 1]]]};
            dimlesscoef[[iii, 2]] = {Total[dimlesscoef[[iii, 2]]]};
            If[Total[dimlesscoef[[iii, 3]]] > 0,
                dimlesscoef[[iii, 3]] = {1};
                ,
                dimlesscoef[[iii, 3]] = {0};
            ];
        ];
        
        (*Drop any summed parts that are 0*)
        newdimlesscoef = {};
        For[izero = 1, izero <= Length[dimlesscoef], izero++,
            If[dimlesscoef[[izero, 1, 1]] == 0,
                dimlesscoef[[izero, 3, 1]] == 0;
                If[Length[dimlesscoef] == 1,
                    newdimlesscoef = Join[newdimlesscoef, {dimlesscoef[[izero]]}];
                ];
                ,
                newdimlesscoef = Join[newdimlesscoef, {dimlesscoef[[izero]]}];
                ,
                newdimlesscoef = Join[newdimlesscoef, {dimlesscoef[[izero]]}];
            ];
        ];
        dimlesscoef = newdimlesscoef;
        
        
        (*Sum the added parts*)
        dimlesstotal = Total[dimlesscoef[[;;, 1, 1]]];
        
        (*Adjust rank*)
        If[Equal @@ dimlesscoef[[;;, 2, 1]],
            dimlessrank = dimlesscoef[[1, 2, 1]];
            ,
            Print["CRITICAL ERROR: " <> functionName <> ": MakeSingleTerm: Tensors without the same rank are being summed together. The term is: ", argumentparts];
            Return[Null];
        ];
        
        (*Adjust dependency*)
        If[Total[dimlesscoef[[;;, 3, 1]]] > 0,
            dimlessdependency = 1;
            ,
            dimlessdependency = 0;
        ];
        
        dimlesscoef = {{{dimlesstotal}, {dimlessrank}, {dimlessdependency}}};
        
        Return[dimlesscoef];
    ];


(*Checked*)
OperatorApplier[term_, bracketform_, subscript_] :=
    Block[{solution},
        If[subscript == {},
            solution = bracketform[[1]] /. {bracketform[[3]] -> term};
            ,
            solution = bracketform[[1]] /. {bracketform[[2]] -> subscript, \
              bracketform[[3]] -> term};
            ,
            solution = bracketform[[1]] /. {bracketform[[2]] -> subscript, \
              bracketform[[3]] -> term};
        ];
        
        Return[solution];
    ];


(*Checked*)
dependencyadder[term_, dependencies_] :=
    Block[{dummy2, i, solution},
        dummy2 = ToString[term];
        If[Length[dependencies] > 0,
            dummy2 = dummy2 <> ToString["["];
            For[i = 1, i <= Length[dependencies], i++,
                If[i == 1,
                    dummy2 = dummy2 <> ToString[dependencies[[i]]];
                    ,
                    dummy2 = dummy2 <> ToString[","] <> ToString[dependencies[[i]]];
                ];
            ];
            dummy2 = dummy2 <> ToString["]"];
        ];
        solution = ToExpression[dummy2];
        
        Return[solution];
    ];


(*Checked*)
listProduct[x_List] :=
    Times @@ x;


End[]

EndPackage[]

