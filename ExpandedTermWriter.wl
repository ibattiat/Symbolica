(* ::Package:: *)

(* Symbolica \[Copyright] 2023 by Kyle Pietrzyk is licensed under Attribution-NonCommercial-ShareAlike 4.0 International *)


(****** ExpandedTermWriter.wl -- Write the long notation of the components of the expanded \
terms (the ones used in the code) given the dependent variable, number of expanded orders, \
 derivative of term, and independent variable dependency.
*************)

(* 

ExpandedTermWriter[DVnIV, Norder, IV, option, derivatives] 

Write the long notation of the components of the expanded \
terms (the ones used in the code) given the dependent variable, derivative of term, and \
independent variable dependency.

*)


(* Created April 22 2020
Modified July 23 2020: Added option of "Add Dependency" to just add a dependency to a DV.

Modified August 24 2020: Updated the "seriesexpansion" function to handle any
                         number of dependencies automatically.
                       *)



BeginPackage["Disruptioneering`ExpandedTermWriter`"];

ExpandedTermWriter::usage = 
"ExpandedTermWriter[DVnIV, Norder, IV, option, derivatives]  Write the long notation of the components of the expanded \
terms (the ones used in the code) given the dependent variable, total order number, derivative \
of term, and independent variable dependency.";



Begin["`Disruptioneering`"];

ExpandedTermWriter[DVnIV_, Norder_, IV_, option_, derivatives_] := 
    Block[{solution, chosen = 0, i},
        If[option == "Individual Prints",
            chosen = 1;
            solution = main2[DVnIV, Norder, IV, option, derivatives];
        ];
        
        If[option == "Add Dependency",
            chosen = 1;
            solution = ConstantArray[{}, Length[DVnIV]];
            For[i = 1, i <= Length[DVnIV], i++,
                solution[[i]] = Join[solution[[i]], {dependencyadder[DVnIV[[i]], IV[[i]]]}];
            ];
        ];
        
        If[chosen == 0,
            solution = main[DVnIV, Norder, IV, option, derivatives];
        ];
        
        Return[solution];
    ];
    


main2[DVnIV_, Norderdummy_, IVdummy_, option_, derivatives_] :=
    Block[{i, ii, j, k, DVnIVdummy, IV, solution, Norder},
        
        If[Length[DVnIV] == 0,
            DVnIVdummy = {DVnIV};
            solution = ConstantArray[0, Length[DVnIVdummy]];
            ,
            DVnIVdummy = DVnIV;
            solution = ConstantArray[0, Length[DVnIVdummy]];
            ,
            Print["main: Error in the input for DVnIV."];
        ];
        
        If[Length[Norderdummy] == 0,
            Norder = {Norderdummy};
            ,
            Norder = Norderdummy;
            ,
            Print["main: Error in the input for norder."];
        ];

        If[Depth[IVdummy] == 2,
            IV = {IVdummy};
            ,
            IV = IVdummy;
            ,
            Print["main: Error in the input for IV."];
        ];

                
        For[ii = 1, ii <= Length[DVnIVdummy], ii++,
            solution[[ii]] = ConstantArray[0, Length[Norder]];
            For[i = 1, i <= Length[Norder], i++,
                solution[[ii, i]] = seriesexpansion[DVnIVdummy[[ii]], IV[[ii]], Norder[[i]]];
            ];
        ];
        
        
        Return[solution];
    ];


main[DVnIV_, Norder_, IVdummy_, option_, derivatives_] :=
    Block[{i, ii, j, k, DVnIVdummy, IV, solution},
        
        If[Length[DVnIV] == 0,
            DVnIVdummy = {DVnIV};
            solution = ConstantArray[0, Length[DVnIVdummy]];
            ,
            DVnIVdummy = DVnIV;
            solution = ConstantArray[0, Length[DVnIVdummy]];
            ,
            Print["main: Error in the input for DVnIV."];
        ];

        If[Depth[IVdummy] == 2,
            IV = {IVdummy};
            ,
            IV = IVdummy;
            ,
            Print["main: Error in the input for IV."];
        ];

                
        For[ii = 1, ii <= Length[DVnIVdummy], ii++,
            solution[[ii]] = ConstantArray[0, Norder + 1];
            For[i = 0, i <= Norder, i++,
                solution[[ii, i + 1]] = seriesexpansion[DVnIVdummy[[ii]], IV[[ii]], i];
            ];
        ];
        
        
        (*Applying Derivatives*)
        If[option == "Individual Derivatives",
            For[j = 1, j <= Length[solution], j++,
                For[ii = 1, ii <= Length[derivatives[[j]]], ii++,
                    solution[[j]] = D[solution[[j]], {IV[[j, ii]], derivatives[[j, ii]]}];
                ];
            ];
        ];
        
        If[option == "All Combinations",
            If[derivatives >= 1,
                For[ii = 1, ii <= Length[solution], ii++,
                    For[i = 1, i <= Length[solution[[ii]]], i++,
                        solution[[ii, i]] = comboendfinder[solution[[ii, i]], IV[[ii]], \
                        derivatives, 1];
                    ];
                ];
                ,
                For[ii = 1, ii <= Length[solution], ii++,
                    For[i = 1, i <= Length[solution[[ii]]], i++,
                        solution[[ii, i]] = {solution[[ii, i]]};
                    ];
                ];
            ];
        ];
        
        Return[solution];
    ];


comboendfinder[term_, IV_, Nderivative_, skipto_] :=
    Block[{i, solution = {}},
        
        If[Nderivative == 0,
            solution = "done";
            ,
            For[i = skipto, i <= Length[IV], i++,
                solution = Join[solution, derivativetaker[term, IV, Nderivative, IV[[i]], i]];
            ];
        ];
        
        Return[solution];
    ];


derivativetaker[term_, IV_, Nderivative_, IVtotake_, skipto_] :=
    Block[{solution = {}, newNderivative = Nderivative - 1, probe},
        
        If[newNderivative == 0,
            solution = {D[term, IVtotake]};
        ];
        
        probe = comboendfinder[D[term, IVtotake], IV, newNderivative, skipto];
        If[probe == "done",
            ,
            solution = Join[solution, probe];
            ,
            solution = Join[solution, probe];
        ];
        
        Return[solution];
    ];


seriesexpansion[a_, IV_, N_] :=
    Block[{i, j, dummy2, solution = 0},
        dummy2 = ToString[a] <> ToString[N];
        If[Length[IV] > 0,
            dummy2 = dummy2 <> ToString["["];
            For[j = 1, j <= Length[IV], j++,
                If[j == 1,
                    dummy2 = dummy2 <> ToString[IV[[j]]];
                    ,
                    dummy2 = dummy2 <> ToString[","] <> ToString[IV[[j]]];
                ];
            ];
            dummy2 = dummy2 <> ToString["]"];
        ];
        solution = ToExpression[dummy2];
        
        Return[solution];
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


End[]

EndPackage[]

