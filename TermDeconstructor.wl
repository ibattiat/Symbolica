(* ::Package:: *)

(* Symbolica \[Copyright] 2023 by Kyle Pietrzyk is licensed under Attribution-NonCommercial-ShareAlike 4.0 International *)


(****** TermDeconstructor.wl --
*************)

(* 

TermDeconstructor[term, ] 

*)


(* Created March 10 2021
Modified : 
                       *)



BeginPackage["Disruptioneering`TermDeconstructor`"];

TermDeconstructor::usage = 
"TermDeconstructor[term ] ";



Begin["`Disruptioneering`"];

TermDeconstructor[term_] := 
    procedure[term, {"Neglect Zeros"}];

TermDeconstructor[term_, option_] := 
    procedure[term, option];


procedure[term_, option_] :=
    Block[{dimlesscoef, solution, term2, globalOption = option},
        
        If[Length[term] > 0,
            If[Head[term[[1]]] == List,
                term2 = term
                ,
                term2 = {term};
                ,
                term2 = {term};
            ];
            ,
            term2 = {term};
        ];
        
        solution = main[term2, {}];
        
        dimlesscoef = main[solution, {2}];
        
        Return[dimlesscoef];
    ];


main[term_, option_] :=
    Block[{solution},
        
        If[option == {},
            If[Head[term] == List,
                solution = caseListmain[term, option];
                ,
                solution = caseNonListmain[term, option];
                ,
                solution = caseNonListmain[term, option];
            ];
            ,
            If[option == {0},
                If[Length[term] == 1,
                    chosen = 0;
                    If[term == {{{0}, {0}, {{}}}} && Intersection[ \
                      globalOption, {"Neglect Zeros"}] == {"Neglect Zeros"},
                        chosen = 1;
                        solution = {{}, {}, {}};
                    ];
                    If[term == {{{-1}, {-1}, {{}}}} && Intersection[ \
                      globalOption, {"Neglect Negative Ones"}] == \
                      {"Neglect Negative Ones"},
                        chosen = 1;
                        solution = {{}, {}, {}};
                    ];
                    
                    If[chosen == 0,
                        solution = {term[[1, 1]], term[[1, 2]], term[[1, 3]]};
                    ];
                ];
                ,
                If[Head[term] == List,
                    solution = caseListmain[term, option];
                    ,
                    solution = caseNonListmain[term, option];
                    ,
                    solution = caseNonListmain[term, option];
                ];
            ];
        ];
        
        Return[solution];
    ];


caseListmain[term_, option_] :=
    Block[{i, solution = ConstantArray[0, Length[term]], dummy},
        
        If[option == {},
            For[i = 1, i <= Length[term], i++,
                solution[[i]] = main[term[[i]], option];
            ];
            ,
            solution = {{}, {}, {}};
            For[i = 1, i <= Length[term], i++,
                dummy = main[term[[i]], {option[[1]] - 1}];
                solution[[1]] = Join[solution[[1]], dummy[[1]]];
                solution[[2]] = Join[solution[[2]], dummy[[2]]];
                solution[[3]] = Join[solution[[3]], dummy[[3]]];
            ];
        ];
        
        Return[solution];
    ];


caseNonListmain[term_, option_] :=
    Block[{i, solution, solutionvec},
        
        If[option == {},
            If[Head[term] == Plus,
                solution = {};
                For[i = 1, i <= Length[term], i++,
                    solutionvec = casePlusSubstituter[term[[i]]];
                    solution = Join[solution, solutionvec];
                ];
                ,
                solution = casePlusSubstituter[term];
                ,
                solution = casePlusSubstituter[term];
            ];
            ,
            Print["CRITICAL ERROR: caseNonListmain: Shouldnt be here..."];
            Return[Null];
        ];
        
        Return[solution];
    ];


casePlusSubstituter[term_] :=
    Block[{solution, levelcountdummy = 0, i},
        
        solution = decider[term, {}];
        
        Return[solution];
    ];


decider[term_, option_] :=
    Block[{dimlesscoef, chosen = 0, i, dependencies},
        
        printouttoggle = 0;
                
        If[printouttoggle == 1,
            Print[term];                
        ];
        
        
        
        If[Head[term] == List && chosen == 0,
            If[printouttoggle == 1,
                Print["Identified List"];                
            ];
            chosen = 1;
            dimlesscoef = caseList[term, option];
        ];
        If[Head[term] == Rational && chosen == 0,
            If[printouttoggle == 1,
                Print["Identified Rational"];                
            ];
            chosen = 1;
            dimlesscoef = caseRational[term, option];
        ];
        If[Head[term] == Integer && chosen == 0,
            If[printouttoggle == 1,
                Print["Identified Integer"];                
            ];
            chosen = 1;
            dimlesscoef = caseInteger[term, option];
        ];
        If[Head[term] == Symbol && chosen == 0,
            If[printouttoggle == 1,
                Print["Identified Symbol"];                
            ];
            chosen = 1;
            dimlesscoef = caseSymbol[term, option];
        ];
        If[Head[term] == Power && chosen == 0,
            If[printouttoggle == 1,
                Print["Identified Power"];                
            ];
            chosen = 1;
            dimlesscoef = casePower[term, option];
        ];
        If[Head[Head[Head[term]]] == Derivative && chosen == 0,
            If[printouttoggle == 1,
                Print["Identified Derivative"];                
            ];
            chosen = 1;
            dimlesscoef = caseDerivative[term, option];
        ];
        If[Head[term] == Times && chosen == 0,
            If[printouttoggle == 1,
                Print["Identified Times"];                
            ];
            chosen = 1;
            dimlesscoef = caseTimes[term, option];
        ];
        If[Head[term] == Plus && chosen == 0,
            If[printouttoggle == 1,
                Print["Identified Plus"];                
            ];
            chosen = 1;
            dimlesscoef = casePlus[term, option];
        ];
        
        If[chosen == 0,
            If[printouttoggle == 1,
                Print["Identified None"];                
            ];
            If[Head[Head[term]] == Symbol,
                dependencies = ConstantArray[0, Length[term]];
                For[i = 1, i <= Length[term], i++,
                    dependencies[[i]] = term[[i]];
                ];
            ];
            dimlesscoef = decider[Head[term], dependencies];
        ];
        
        
        
        If[printouttoggle == 1,
            Print[dimlesscoef];
        ];
        
        Return[dimlesscoef];
        
    ];


caseList[term_, option_] :=
    Block[{},
        Print["CRITICAL ERROR: caseList: Not written yet (but there shouldnt be any lists). Term is: ", term];
        Return[Null];
    ];


caseRational[term_, option_] :=
    Block[{dimlesscoef = {{{term}, {term}, {{}}}}},
        Return[dimlesscoef];
    ];


caseInteger[term_, option_] :=
    Block[{dimlesscoef = {{{term}, {term}, {{}}}}},
        Return[dimlesscoef];
    ];


caseSymbol[term_, option_] :=
    Block[{dimlesscoef, i, term2},
        
        If[option == {},
            dimlesscoef = {{{term}, {term}, {{}}}};
            ,
            term2 = dependencyadder[term, option];
            dimlesscoef = {{{term2}, {term}, {option}}};
            ,
            Print["CRITICAL ERROR: caseSymbol: variable 'option' was not identified. Term was: ", term];
            Return[Null];
        ];
        
        Return[dimlesscoef];
    ];


casePower[term_, option_] :=
    Block[{dimlesscoef, power = term[[2]], i},
        dimlesscoef = decider[term[[1]], option];
        If[Length[dimlesscoef] == 1,
            For[i = 1, i <= Length[dimlesscoef[[1, 1]]], i++,
                dimlesscoef[[1, 1, i]] = dimlesscoef[[1, 1, i]]^power;
            ];
            ,
            Print["CRITICAL ERROR: casePower: argument came back as a sum. Term was: ", term];
            Return[Null];
        ];
        
        Return[dimlesscoef];
    ];


caseDerivative[term_, option_] :=
    Block[{dimlesscoef = 1, dependencies = ConstantArray[0, Length[term]], i, j, \
    termargument = term[[0, 1]]},
        
        For[i = 1, i <= Length[term], i++,
            dependencies[[i]] = term[[i]];
        ];
        
        dimlesscoef = {{{term}, {termargument}, {dependencies}}};
        
        Return[dimlesscoef];
    ];


caseTimes[term_, option_] :=
    Block[{dimlesscoef = {{{}, {}, {}}}, dummy, i, ii},
        
        For[i = 1, i <= Length[term], i++,
            dummy = decider[term[[i]], option];
            For[ii = 1, ii <= Length[dummy[[1, 1]]], ii++,
                If[dummy[[1, 1, ii]] == -1,
                    ,
                    dimlesscoef[[1, 1]] = Join[dimlesscoef[[1, 1]], {dummy[[1, 1, ii]]}];
                    dimlesscoef[[1, 2]] = Join[dimlesscoef[[1, 2]], {dummy[[1, 2, ii]]}];
                    dimlesscoef[[1, 3]] = Join[dimlesscoef[[1, 3]], {dummy[[1, 3, ii]]}];
                    ,
                    dimlesscoef[[1, 1]] = Join[dimlesscoef[[1, 1]], {dummy[[1, 1, ii]]}];
                    dimlesscoef[[1, 2]] = Join[dimlesscoef[[1, 2]], {dummy[[1, 2, ii]]}];
                    dimlesscoef[[1, 3]] = Join[dimlesscoef[[1, 3]], {dummy[[1, 3, ii]]}];
                ];
            ];
        ];
        
        Return[dimlesscoef];
    ];


casePlus[term_, option_] :=
    Block[{},
        Print["CRITICAL ERROR: casePlus: Not written yet. Term is: ", term];
        Return[Null];
    ];


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


listProduct[x_List] :=
    Times @@ x;


End[]

EndPackage[]

