(* ::Package:: *)

(* Symbolica \[Copyright] 2023 by Kyle Pietrzyk is licensed under Attribution-NonCommercial-ShareAlike 4.0 International *)


(****** DimlessIdentifier.mth -- Identify groups of dimensionless numbers *************)

(* 

DimlessIdentifier[Dimensionless Coefficient, Dimensionless Numbers,
	Dimensionless Number Definitions] 

Given a coefficient, a list of dimensionless numbers and their forms, the code 
simplifies the coefficient to a combination of the dimensionless numbers

*)


(* Created March 20 2020
Modified July 28 2020: *)



BeginPackage["Disruptioneering`DimlessIdentifier`"];

DimlessIdentifier::usage = 
"DimlessIdentifier[Dimensionless Coefficient, Dimensionless Numbers,
Dimensionless Number Definitions]

Given a coefficient, a list of dimensionless numbers and their forms, the code 
simplifies the coefficient to a combination of the dimensionless numbers";



Begin["`Disruptioneering`"];

DimlessIdentifier[term_, allscales_, namesDC_, formDC_, mastervariables_] := 
    Block[{solution, previoussolutions, unknownprevioussolutions, newprevioussolutions, \
    dimlessvarcount, dimensionlessnumbersreturn, dimensionlessnumbersformreturn, \
    coords2, scalesglobal, functioninput, coords3, dimensionlessnumbersscalereturn, \
    scalestocheck, uniquescales, symbolsforscales, symbolcount, potentialdimlessnumbers, \
    symbolsforscalescountglobal, coords4, newformDC, i, maxnumberofscalesinoneterm, \
    uniquedummyscales, j, correspondinguniquedummyscales, newformDCscales, identifiedscales, \
    uniquemastervariables, merp},
        
        
        {symbolsforscales, scalestocheck} = groupscalesbymastervariables[allscales, \
          mastervariables];
        
        (*Find the unique scales within the scales provided in SIVS, DVS, velocityS, ...*)
        uniquescales = finduniquescales[scalestocheck];
        
        (*Create all possilibities of Dimensionless numbers using the forms and scales provided*)
        symbolcount = findpossibledimensionlessnumbers[uniquescales, formDC, symbolsforscales];
        potentialdimlessnumbers = createpotentialdimlessnumbers[uniquescales, formDC, \
          symbolsforscales, symbolcount];
        
        
        (*Find what the forms of the provided dimensionless numbers (dimensional form) are
        and keep track of how many of each type of scale are in each dimensionless number.
        For example, eta = C/CC has 2 concentration scales*)
        symbolsforscalescountglobal = term * 0;
        coords4 = {};
        scalesglobal = predecider[term, scalestocheck, symbolsforscales, namesDC, formDC, \
          symbolsforscales];
        
        
        
        
        (*With multiple scales of a single type in a dimensionless number, we need to create
        more forms, since (Cmod1 kmod1 Lmod1^2)/Dmod1 and (Cmod2 kmod1 Lmod1^2)/Dmod1 are
        technically the same, but the computer cannot recognize them as such with Cmod1 and
        Cmod2 as variables.*)
        maxnumberofscalesinoneterm = ConstantArray[0, Length[symbolsforscales]];
        uniquedummyscales = ConstantArray[{}, Length[symbolsforscales]];
        correspondinguniquedummyscales = ConstantArray[{}, Length[symbolsforscales]];
        
        For[i = 1, i <= Length[symbolsforscales], i++,
            maxnumberofscalesinoneterm[[i]] = Max[Join[Flatten[symbolcount[[;;, ;;, i]]], \
              Flatten[symbolsforscalescountglobal[[;;, ;;, ;;, i]]]]];
            For[j = 1, j <= maxnumberofscalesinoneterm[[i]], j++,
                uniquedummyscales[[i]] = Join[uniquedummyscales[[i]], {Symbol[ToString[ \
                  symbolsforscales[[i]]] <> ToString[symbolsforscales[[i]]] <> \
                  ToString[j]]}];
                correspondinguniquedummyscales[[i]] = Join[ \
                  correspondinguniquedummyscales[[i]], {Symbol[ToString[ \
                  symbolsforscales[[i]]] <> ToString[j]]}];
            ];
        ];
        
        (*Using preexisting functions to create the new forms.*)
        newformDC = createpotentialdimlessnumbers[uniquedummyscales, formDC, \
          symbolsforscales, symbolcount];
        
        (*Actually, subbing in Cmod1, etc. to create the new forms.*)
        For[i = 1, i <= Length[newformDC], i++,
            newformDC[[i]] = Flatten[newformDC[[i]]];
            For[j = 1, j <= Length[Flatten[uniquedummyscales]], j++,
                newformDC[[i]] = newformDC[[i]] /. Flatten[uniquedummyscales][[j]] -> \
                  Flatten[correspondinguniquedummyscales][[j]];
            ];
        ];
        
        
        
        (*Now we are going to remake the list of possible dimensionless numbers with, since we 
        now have more forms. First, we identify the scales (Cmod1, etc.) in the new forms
        and record them with their identity, meaning a vector {1,2,3,4,5}, which
        corresponds to {Lmod,Cmod,Umod,Dmod,kmod}*)
        {newformDCscales, identifiedscales} = getnewscalesinforms[newformDC, symbolsforscales];
        
        
        
        
        (*Now we actually create the new potential dimensionless numbers in light of the 
        new forms.*)
        potentialdimlessnumbers = createpotentialdimlessnumbers2[uniquescales, newformDC, \
          newformDCscales, identifiedscales];
        
        
        
        
        (*Now we try our first attempt to subin dimensionless numbers. This is a direct
        substitution that will work if the dimensionless number is obvious/not multiplied by
        another dimensionless number.*)
        functioninput = term;        
        previoussolutions = scalesglobal * 0;
        unknownprevioussolutions = scalesglobal * 0;
        newprevioussolutions = {};
        dimlessvarcount = ConstantArray[0, Length[namesDC]];
        
        dimensionlessnumbersreturn = {};
        dimensionlessnumbersformreturn = {};
        dimensionlessnumbersscalereturn = {};
        
        coords2 = {};
        
        DimensionlessNumberChecker[scalesglobal, uniquescales, namesDC, newformDC, \
          symbolsforscales, 0];
        
        
        
        
        (*Now we try the second attempt to subin dimensionless number, which will try every
        combination of multiplying and dividing the numbers to get the dimensionless form to
        be 1. Then, all combinations of the possible dimensionless numbers (of the forms that
        worked) are put together to try and find the correct combination of the dimensionless
        number. If this does not work, either allow for more combinations of multiplying and
        dividing dimensionless numbers (Ntest), or you did not supply all the scales or forms*)
        
        coords3 = {};
        
        DimensionlessNumberCheckerLastAttempt[previoussolutions, uniquescales, namesDC, \
          newformDC, symbolsforscales, 0];
                
        Return[{previoussolutions, dimensionlessnumbersreturn, \
          dimensionlessnumbersformreturn, dimensionlessnumbersscalereturn, dimlessvarcount}];
    ];


groupscalesbymastervariables[scales_, mastervariables_] :=
    Block[{i, j, flag, uniqueMV = {}, MVscales = {}},
        For[i = 1, i <= Length[mastervariables], i++,
            flag = 0;
            For[j = 1, j <= Length[uniqueMV], j++,
                If[uniqueMV[[j]] == mastervariables[[i]],
                    MVscales[[j]] = Join[MVscales[[j]], {scales[[i]]}];
                    flag = 1;
                    Break[];
                ];
            ];
            If[flag == 0,
                uniqueMV = Join[uniqueMV, {mastervariables[[i]]}];
                MVscales = Join[MVscales, {{scales[[i]]}}];
            ];
        ];
        
        Return[{uniqueMV, MVscales}];
    ];


finduniquescales[scalestocheck_] :=
    Block[{i, j, k, uniquescales, flag = 0},
        uniquescales = ConstantArray[{}, Length[scalestocheck]];
        For[i = 1, i <= Length[scalestocheck], i++,
            For[j = 1, j <= Length[scalestocheck[[i]]], j++,
                flag = 0;
                For[k = 1, k <= Length[uniquescales[[i]]], k++,
                    If[uniquescales[[i, k]] == scalestocheck[[i, j]],
                        flag = 1;
                        Break[];
                    ];
                ];
                If[flag == 0,
                    uniquescales[[i]] = Join[uniquescales[[i]], {scalestocheck[[i, j]]}];
                ];
            ];
        ];
        
        Return[uniquescales];
    ];


findpossibledimensionlessnumbers[uniquescales_, formDC_, symbolsforscales_] :=
    Block[{i, j, k, symbolcount, counter, testsymbol, zerotest},
        
        symbolcount = ConstantArray[{}, Length[formDC]];
        For[i = 1, i <= Length[formDC], i++,
            symbolcount[[i]] = Join[symbolcount[[i]], ConstantArray[{}, \
              Length[formDC[[i]]]]];
            For[j = 1, j <= Length[formDC[[i]]], j++,
                symbolcount[[i, j]] = Join[symbolcount[[i, j]], ConstantArray[0, \
                  Length[symbolsforscales]]];
                For[k = 1, k <= Length[symbolsforscales], k++,
                    counter = 0;
                    While[1 > 0,
                        testsymbol = Symbol[ToString[symbolsforscales[[k]]] <> \
                          ToString[counter + 1]];
                        zerotest = Quiet[Check[formDC[[i, j]] /. testsymbol -> 0, 0]] /. \
                          {0^a_ -> 0};
                        If[zerotest == 0,
                            counter += 1;
                            ,
                            Break[];
                            ,
                            Break[];
                        ];
                    ];
                    symbolcount[[i, j, k]] = counter;
                ];
                If[Total[symbolcount[[i, j]]] != Length[formDC[[i, j]]],
                    symbolcount[[i, j]] = ConstantArray[0, Length[symbolcount[[i, j]]]];
                ];
            ];
        ];
    
        Return[symbolcount];
    ];


createpotentialdimlessnumbers[uniquescales_, formDC_, symbolsforscales_, symbolcount_] :=
    Block[{potentialdimlessnumbers, i, j, k},
        
        potentialdimlessnumbers = ConstantArray[{}, Length[formDC]];
        For[i = 1, i <= Length[formDC], i++,
            potentialdimlessnumbers[[i]] = ConstantArray[0, Length[formDC[[i]]]];
            For[j = 1, j <= Length[formDC[[i]]], j++,
                If[Length[symbolcount[[i, j]]] > 0,
                    potentialdimlessnumbers[[i, j]] = Flatten[potentialdimlessnumbersdeterminer[ \
                      formDC[[i, j]], uniquescales, symbolsforscales, symbolcount[[i, j]], \
                      ConstantArray[{0}, Length[symbolcount[[i, j]]]]]];
                    ,
                    potentialdimlessnumbers[[i, j]] = {};
                ];
            ];
        ];
        
        Return[potentialdimlessnumbers];
    ];


potentialdimlessnumbersdeterminer[formDC_, uniquescales_, symbolsforscales_, symbolcount_, \
previouslyuseduniquescales_] :=
    Block[{dummysymbolcount = symbolcount, i, j, k, newformDC, chosen = 0, solution, subin, \
    flag = 0, dummypreviouslyuseduniquescales = previouslyuseduniquescales},
        
        For[i = 1, i <= Length[dummysymbolcount], i++,
            If[dummysymbolcount[[i]] > 0,
                chosen = 1;
                subin = Symbol[ToString[symbolsforscales[[i]]] <> \
                  ToString[dummysymbolcount[[i]]]];
                dummysymbolcount[[i]] -= 1;
                solution = {};
                
                For[j = 1, j <= Length[uniquescales[[i]]], j++,
                    flag = 0;
                    For[k = 1, k <= Length[dummypreviouslyuseduniquescales[[i]]], k++,
                        If[dummypreviouslyuseduniquescales[[i, k]] == j,
                            flag = 1;
                            Break[];
                        ];
                    ];
                    
                    If[flag == 1,
                        ,
                        dummypreviouslyuseduniquescales[[i]] = Join[\
                          dummypreviouslyuseduniquescales[[i]], {j}];
                        newformDC = formDC /. subin -> uniquescales[[i, j]];
                        solution = Join[solution, {0}];
                        solution[[-1]] = potentialdimlessnumbersdeterminer[newformDC, \
                          uniquescales, symbolsforscales, dummysymbolcount, \
                          dummypreviouslyuseduniquescales];
                        
                        dummypreviouslyuseduniquescales[[i]] = Drop[dummypreviouslyuseduniquescales[[i]], -1];
                    ];
                ];
                
                Break[];
            ];
        ];
        
        If[chosen == 0,
            solution = formDC;
        ];
        
        Return[solution];
    ];


getnewscalesinforms[newformDC_, symbolsforscales_] :=
    Block[{i, j, k, newformDCscales, chosen = 0, kk, count = 0, counterlimit = 10, \
    identifiedscale, flag1 = 0, potentialscales},
        newformDCscales = ConstantArray[{}, Length[newformDC]];
        identifiedscale = ConstantArray[{}, Length[newformDC]];
        For[i = 1, i <= Length[newformDC], i++,
            newformDCscales[[i]] = ConstantArray[{}, Length[newformDC[[i]]]];
            identifiedscale[[i]] = ConstantArray[{}, Length[newformDC[[i]]]];
            For[j = 1, j <= Length[newformDC[[i]]], j++,
                
                chosen = 0;
                If[newformDC[[i, j, 0]] == Times,
                    chosen = 1;
                    newformDCscales[[i, j]] = ConstantArray[0, Length[newformDC[[i, j]]]];
                    identifiedscale[[i, j]] = ConstantArray[0, Length[newformDC[[i, j]]]];
                    For[k = 1, k <= Length[newformDC[[i, j]]], k++,
                        If[newformDC[[i, j, k, 0]] == Power,
                            newformDCscales[[i, j, k]] = newformDC[[i, j, k, 1]];
                            ,
                            newformDCscales[[i, j, k]] = newformDC[[i, j, k]];
                            ,
                            newformDCscales[[i, j, k]] = newformDC[[i, j, k]];
                        ];
                    ];
                ];
                If[newformDC[[i, j, 0]] == Power,
                    chosen = 1;
                    newformDCscales[[i, j]] = ConstantArray[0, 1];
                    identifiedscale[[i, j]] = ConstantArray[0, 1];
                    newformDCscales[[i, j, 1]] = newformDC[[i, j, 1]];
                ];
                If[chosen == 0,
                    Print["CRITICAL ERROR: getnewscalesinforms: Could not identify the form"];
                    Return[Null];
                ];
                
                                
                For[k = 1, k <= Length[newformDCscales[[i, j]]], k++,
                    count = 0;
                    flag1 = 0;
                    While[counterlimit > count,
                        For[kk = 1, kk <= Length[symbolsforscales], kk++,
                            potentialscales = Symbol[ToString[symbolsforscales[[kk]]] <> \
                              ToString[count]];
                            If[newformDCscales[[i, j, k]] /. potentialscales -> 0 == 0,
                                identifiedscale[[i, j, k]] = kk;
                                flag1 = 1;
                                Break[];
                            ];
                        ];
                        If[flag1 == 1,
                            flag1 = 0;
                            Break[];
                            ,
                            count += 1;
                        ];
                    ];
                ];
                
            ];
        ];
        
        Return[{newformDCscales, identifiedscale}];
    ];


createpotentialdimlessnumbers2[uniquescales_, newformDC_, \
newformDCscales_, identifiedscales_] :=
    Block[{potentialdimlessnumbers, i, j, k},
        
        potentialdimlessnumbers = ConstantArray[{}, Length[newformDC]];
        For[i = 1, i <= Length[newformDC], i++,
            potentialdimlessnumbers[[i]] = ConstantArray[0, Length[newformDC[[i]]]];
            For[j = 1, j <= Length[newformDC[[i]]], j++,
                potentialdimlessnumbers[[i, j]] = Flatten[potentialdimlessnumbersdeterminer2[ \
                  newformDC[[i, j]], uniquescales, \
                  ConstantArray[1, Length[newformDCscales[[i, j]]]], \
                  newformDCscales[[i, j]], \
                  ConstantArray[{0}, Length[uniquescales]], \
                  identifiedscales[[i, j]]]];
            ];
        ];
        
        Return[potentialdimlessnumbers];
    ];


potentialdimlessnumbersdeterminer2[formDC_, uniquescales_, symbolcount_, \
newformDCscales_, previouslyuseduniquescales_, identifiedscales_] :=
    Block[{dummysymbolcount = symbolcount, i, j, k, newformDC, chosen = 0, solution, subin, \
    flag = 0, dummypreviouslyuseduniquescales = previouslyuseduniquescales},
        
        For[i = 1, i <= Length[dummysymbolcount], i++,
            If[dummysymbolcount[[i]] > 0,
                chosen = 1;
                subin = newformDCscales[[i]];
                dummysymbolcount[[i]] -= 1;
                solution = {};
                
                For[j = 1, j <= Length[uniquescales[[identifiedscales[[i]]]]], j++,
                    flag = 0;
                    For[k = 1, k <= Length[dummypreviouslyuseduniquescales[[ \
                      identifiedscales[[i]]]]], k++,
                        If[dummypreviouslyuseduniquescales[[identifiedscales[[i]], k]] == j,
                            flag = 1;
                            Break[];
                        ];
                    ];
                    
                    If[flag == 1,
                        ,
                        dummypreviouslyuseduniquescales[[identifiedscales[[i]]]] = Join[\
                          dummypreviouslyuseduniquescales[[identifiedscales[[i]]]], {j}];
                        newformDC = formDC /. subin -> uniquescales[[identifiedscales[[i]], j]];
                        solution = Join[solution, {0}];
                        solution[[-1]] = potentialdimlessnumbersdeterminer2[newformDC, \
                          uniquescales, dummysymbolcount, newformDCscales, \
                          dummypreviouslyuseduniquescales, identifiedscales];
                        dummypreviouslyuseduniquescales[[identifiedscales[[i]]]] = \
                          Drop[dummypreviouslyuseduniquescales[[identifiedscales[[i]]]], -1];
                    ];
                ];
                
                Break[];
            ];
        ];
        
        If[chosen == 0,
            solution = formDC;
        ];
        
        Return[solution];
    ];


DimensionlessNumberChecker[scales_, uniquescales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{},
        If[Head[scales] == List,
            caseListDimensionlessNumbers[scales, uniquescales, namesDC, formDC, dummyS, \
              levelcountdummy];
            ,
            caseNonListDimensionlessNumbers[scales, uniquescales, namesDC, formDC, dummyS, \
              levelcountdummy];
            ,
            caseNonListDimensionlessNumbers[scales, uniquescales, namesDC, formDC, dummyS, \
              levelcountdummy];
        ];
    ];


caseListDimensionlessNumbers[scales_, uniquescales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{i, coordinate2},
        
        coords2 = Append[coords2, 1];
        
        For[i = 1, i <= Length[scales], i++,
            coords2[[-1]] = i;
            DimensionlessNumberChecker[scales[[i]], uniquescales, namesDC, formDC, dummyS, \
              levelcountdummy];
        ];
        
        coords2 = Drop[coords2, -1];
    ];


caseNonListDimensionlessNumbers[scales_, uniquescales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{flag = 0, flag2 = 0, j, jj, solution = 0, zerosanddimlessnumbers, dummyy},
        
        For[jj = 1, jj <= Length[namesDC], jj++,
            For[j = 1, j <= Length[formDC[[jj]]], j++,
                If[scales == formDC[[jj, j]],
                
                    (*Check for declarations on other scales that are the same*)
                    coords = {};
                    zerosanddimlessnumbers = UsedDimensionlessNumberChecker[ \
                      previoussolutions, scales, uniquescales, namesDC, formDC, dummyS, \
                        levelcountdummy];
                    If[zerosanddimlessnumbers[[1]] == 0,
                        dimlessvarcount[[jj]] += 1;
                        previoussolutions[[coords2[[1]], coords2[[2]], coords2[[3]]]] = \
                          Symbol[ToString[namesDC[[jj]]] <> ToString[dimlessvarcount[[jj]]]];
                        dimensionlessnumbersreturn = Join[dimensionlessnumbersreturn, \
                          {previoussolutions[[coords2[[1]], coords2[[2]], coords2[[3]]]]}];
                        dimensionlessnumbersformreturn = Join[dimensionlessnumbersformreturn, \
                          {scales}];
                        dimensionlessnumbersscalereturn = Join[ \
                          dimensionlessnumbersscalereturn, {functioninput[[coords2[[1]], \
                          coords2[[2]], coords2[[3]]]]}];
                        ,
                        previoussolutions[[coords2[[1]], coords2[[2]], coords2[[3]]]] = \
                          zerosanddimlessnumbers[[1]];
                        ,
                        previoussolutions[[coords2[[1]], coords2[[2]], coords2[[3]]]] = \
                          zerosanddimlessnumbers[[1]];
                    ];
                    
                    flag = 1;
                    Break[];
                ];
            ];
            If[flag == 1,
               flag = 0;
               flag2 = 1;
               Break[];
            ];         
        ];
        
        If[flag2 == 0,
            previoussolutions[[coords2[[1]], coords2[[2]], coords2[[3]]]] = scales;
            unknownprevioussolutions[[coords2[[1]], coords2[[2]], coords2[[3]]]] = 1;
            ,
            flag2 = 0;
        ];
        
        If[printouttoggle == 1,
            Print[previoussolutions];                
        ];
    ];


UsedDimensionlessNumberChecker[prevsol_, scales_, uniquescales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{solution},
        If[Head[prevsol] == List,
            solution = caseListPreviousSolutions[prevsol, scales, uniquescales, namesDC, \
              formDC, dummyS, levelcountdummy];
            ,
            solution = caseNonListPreviousSolutions[prevsol, scales, uniquescales, namesDC, \
              formDC, dummyS, levelcountdummy];
            ,
            solution = caseNonListPreviousSolutions[prevsol, scales, uniquescales, namesDC, \
              formDC, dummyS, levelcountdummy];
        ];
        
        Return[solution];
    ];


caseListPreviousSolutions[prevsol_, scales_, uniquescales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{solution = {0}, i, coordinate, output},
        
        coords = Append[coords, 1];
        
        For[i = 1, i <= Length[prevsol], i++,
            coords[[-1]] = i;
            output = UsedDimensionlessNumberChecker[prevsol[[i]], scales, uniquescales, \
              namesDC, formDC, dummyS, levelcountdummy];
            
            If[output[[1]] == 0,
                ,
                solution = output;
                Break[];
                ,
                solution = output;
                Break[];
            ];
        ];
        
        coords = Drop[coords, -1];
        
        Return[solution];
    ];


caseNonListPreviousSolutions[prevsol_, scales_, uniquescales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{solution},
        
        If[functioninput[[coords2[[1]], coords2[[2]], coords2[[3]]]] == \
          functioninput[[coords[[1]], coords[[2]], coords[[3]]]],
            solution = {previoussolutions[[coords[[1]], coords[[2]], coords[[3]]]]};
            ,
            solution = {0};
            ,
            solution = {0};
        ];
        
        Return[solution];
    ];


DimensionlessNumberCheckerLastAttempt[scales_, uniquescales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{},
        If[Head[scales] == List,
            caseListDimensionlessNumbersLastAttempt[scales, uniquescales, namesDC, formDC, \
              dummyS, levelcountdummy];
            ,
            caseNonListDimensionlessNumbersLastAttempt[scales, uniquescales, namesDC, formDC, \
              dummyS, levelcountdummy];
            ,
            caseNonListDimensionlessNumbersLastAttempt[scales, uniquescales, namesDC, formDC, \
              dummyS, levelcountdummy];
        ];
    ];


caseListDimensionlessNumbersLastAttempt[scales_, uniquescales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{i, coordinate3},
        
        coords3 = Append[coords3, 1];
        
        For[i = 1, i <= Length[scales], i++,
            coords3[[-1]] = i;
            DimensionlessNumberCheckerLastAttempt[scales[[i]], uniquescales, namesDC, formDC, \
              dummyS, levelcountdummy];
        ];
        
        coords3 = Drop[coords3, -1];
    ];


caseNonListDimensionlessNumbersLastAttempt[term_, uniquescales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{solution, i, j, Ntests = 3},
        
        If[printouttoggle == 1,
            Print[term];
        ];
        
        
        If[unknownprevioussolutions[[coords3[[1]], coords3[[2]], coords3[[3]]]] == 1,
            If[term == 1 || term == 0,
                ,
                For[i = 1, i <= Ntests, i++,
                    solution = multdivtest[term, uniquescales, namesDC, formDC, dummyS, \
                      levelcountdummy, i, 0];
                    If[solution == {{0, 0}},
                        ,
                        previoussolutions[[coords3[[1]], coords3[[2]], coords3[[3]]]] = \
                          listProduct[solution];
                        Break[];
                        ,
                        previoussolutions[[coords3[[1]], coords3[[2]], coords3[[3]]]] = \
                          listProduct[solution];
                        Break[];
                    ];
                ];
                ,
                For[i = 1, i <= Ntests, i++,
                    solution = multdivtest[term, uniquescales, namesDC, formDC, dummyS, \
                      levelcountdummy, i, 0];
                    If[solution == {{0, 0}},
                        ,
                        previoussolutions[[coords3[[1]], coords3[[2]], coords3[[3]]]] = \
                          listProduct[solution];
                        Break[];
                        ,
                        previoussolutions[[coords3[[1]], coords3[[2]], coords3[[3]]]] = \
                          listProduct[solution];
                        Break[];
                    ];
                ];
            ];
        ];
        
    ];


multdivtest[term_, uniquescales_, namesDC_, formDC_, dummyS_, levelcountdummy_, Ntests_, \
counter_] :=
    Block[{i, ii, j, solution, solution2, flag = 0, count = counter, termdiv, \
    chosen = 0, firstmultdiv = 0, checksolutionvec, chosen2 = 0},
        
        If[counter == 0,
            firstmultdiv = 1;
        ];
        count += 1;
        
        For[ii = 1, ii <= 2, ii++, (*1 = mult, 2 = divide*)
            For[i = 1, i <= Length[namesDC], i++,
                For[j = 1, j <= Length[formDC[[i]]], j++,
                    
                    If[ii == 1,
                        termdiv = term / formDC[[i, j]];
                        ,
                        termdiv = term * formDC[[i, j]];
                    ];
                    
                    
                    If[termdiv == 1,
                        chosen2 = 1;
                        flag = 1;
                        If[ii == 1,
                            solution = {{namesDC[[i]], formDC[[i, j]]}};
                            ,
                            solution = {{namesDC[[i]]^-1, formDC[[i, j]]^-1}};
                        ];
                        
                        If[firstmultdiv == 1,
                            solution = Checkifsolutioniscorrect[solution, term, namesDC, \
                              formDC];
                            If[solution == term,
                                ,
                                Break[];
                                ,
                                Break[];
                            ];
                            ,
                            Break[];
                        ];
                    ];
                    
                    
                    If[chosen2 == 0,
                        If[count < Ntests,
                            solution = multdivtest[termdiv, uniquescales, namesDC, formDC, \
                              dummyS, levelcountdummy, Ntests, count];
                            If[solution[[1]] == {0, 0},
                                ,
                                
                                flag = 1;
                                If[ii == 1,
                                    solution2 = {namesDC[[i]], formDC[[i, j]]};
                                    ,
                                    solution2 = {namesDC[[i]]^-1, formDC[[i, j]]^-1};
                                ];
                                solution = Join[solution, {solution2}];
                                
                                If[firstmultdiv == 1,
                                    solution = Checkifsolutioniscorrect[solution, term, \
                                      namesDC, formDC];
                                    If[solution == term,
                                        ,
                                        Break[];
                                        ,
                                        Break[];
                                    ];
                                    ,
                                    Break[];
                                ];
                                
                                ,
                                
                                flag = 1;
                                If[ii == 1,
                                    solution2 = {namesDC[[i]], formDC[[i, j]]};
                                    ,
                                    solution2 = {namesDC[[i]]^-1, formDC[[i, j]]^-1};
                                ];
                                solution = Join[solution, {solution2}];
                                
                                If[firstmultdiv == 1,
                                    solution = Checkifsolutioniscorrect[solution, term, \
                                      namesDC, formDC];
                                    If[solution == term,
                                        ,
                                        Break[];
                                        ,
                                        Break[];
                                    ];
                                    ,
                                    Break[];
                                ];
                            ];
                            
                            ,
                            
                            solution = {{0, 0}};
                            
                            ,
                            
                            Print["CRITICAL ERROR: multdivtest: Something went wrong."];
                            Return[Null];
                        ];
                    ];
                ]; 
                If[flag == 1,
                    Break[];
                ];
            ];
            If[flag == 1,
                Break[];
            ];
        ];
        
        If[flag == 0,
            solution = {{0, 0}};
        ];
        
        Return[solution];
    ];


Checkifsolutioniscorrect[solutioninput_, term_, namesDC_, formDC_] :=
    Block[{solution, checksolutionvec, i, j, chosen2 = 0, chosen = 0, dimlesscoef, flag1 = 0},
        
        If[solutioninput[[1]] == {0, 0},
            chosen2 = 1;
            dimlesscoef = term;
        ];
        
        If[chosen2 == 0,
            checksolutionvec = ConstantArray[0, Length[solutioninput]];
            
            solution = dimlessnumbertryer[solutioninput, checksolutionvec, namesDC, formDC];
            
            If[solution == 0,
                chosen = 1;
                dimlesscoef = {{0, 0}};
            ];
            
            If[chosen == 0,
                dimlesscoef = ConstantArray[0, Length[solution]];
                For[i = 1, i <= Length[solution], i++,
                    flag1 = 0;
                    For[j = 1, j <= Length[dimensionlessnumbersscalereturn], j++,
                        If[solution[[i, 3]] == dimensionlessnumbersscalereturn[[j]],
                            flag1 = 1;
                            dimlesscoef[[i]] = dimensionlessnumbersreturn[[j]];
                            Break[];
                        ];
                        If[solution[[i, 3]] == dimensionlessnumbersscalereturn[[j]]^-1,
                            flag1 = 1;
                            dimlesscoef[[i]] = dimensionlessnumbersreturn[[j]]^-1;
                            Break[];
                        ];
                    ];
                    
                    If[flag1 == 0,
                        For[j = 1, j <= Length[namesDC], j++,
                            If[namesDC[[j]] == solution[[i, 1]],
                                dimlessvarcount[[j]] += 1;
                                dimensionlessnumbersreturn = Join[dimensionlessnumbersreturn, \
                                  {Symbol[ToString[namesDC[[j]]] <> ToString[ \
                                  dimlessvarcount[[j]]]]}];
                                dimensionlessnumbersformreturn = Join[ \
                                  dimensionlessnumbersformreturn, {solution[[i, 2]]}];
                                dimensionlessnumbersscalereturn = Join[ \
                                  dimensionlessnumbersscalereturn, {solution[[i, 3]]}];
                                If[solution[[i, 4]] == 1,
                                    dimlesscoef[[i]] = dimensionlessnumbersreturn[[-1]];
                                    ,
                                    dimlesscoef[[i]] = 1 / dimensionlessnumbersreturn[[-1]];
                                    ,
                                    dimlesscoef[[i]] = dimensionlessnumbersreturn[[-1]];
                                ];
                            ];
                        ];
                    ];
                    
                ];
            ];
            
        ];
        
        Return[dimlesscoef];
    ];


dimlessnumbertryer[solutioninput_, checksolutionvec_, namesDC_, formDC_] :=
    Block[{dummychecksolutionvec = checksolutionvec, i, j, jj, k, chosen = 0, solution, flag1 = 0},
        
        For[i = 1, i <= Length[dummychecksolutionvec], i++,
            If[dummychecksolutionvec[[i]] == 0,
                chosen = 1;
                
                For[j = 1, j <= Length[namesDC], j++,
                    
                    If[solutioninput[[i, 1]] == namesDC[[j]],
                        For[jj = 1, jj <= Length[formDC[[j]]], jj++,
                            If[solutioninput[[i, 2]] == formDC[[j, jj]],
                                For[k = 1, k <= Length[potentialdimlessnumbers[[j, jj]]], k++,
                                    flag1 = 1;
                                    dummychecksolutionvec[[i]] = \
                                      potentialdimlessnumbers[[j, jj, k]];
                                    solution = dimlessnumbertryer[solutioninput, \
                                      dummychecksolutionvec, namesDC, formDC];
                                    If[solution == 0,
                                        ,
                                        solution = Join[solution, {{namesDC[[j]], \
                                          formDC[[j, jj]], \
                                          potentialdimlessnumbers[[j, jj, k]], 1}}];
                                        Return[solution];
                                        ,
                                        solution = Join[solution, {{namesDC[[j]], \
                                          formDC[[j, jj]], \
                                          potentialdimlessnumbers[[j, jj, k]], 1}}];
                                        Return[solution];
                                    ];
                                ];
                            ];
                        ];
                    ];
                    
                    If[solutioninput[[i, 1]] == namesDC[[j]]^-1,
                        For[jj = 1, jj <= Length[formDC[[j]]], jj++,
                            If[solutioninput[[i, 2]] == formDC[[j, jj]]^-1,
                                For[k = 1, k <= Length[potentialdimlessnumbers[[j, jj]]], k++,
                                    flag1 = 1;
                                    dummychecksolutionvec[[i]] = \
                                      potentialdimlessnumbers[[j, jj, k]]^-1;
                                    solution = dimlessnumbertryer[solutioninput, \
                                      dummychecksolutionvec, namesDC, formDC];
                                    If[solution == 0,
                                        ,
                                        solution = Join[solution, {{namesDC[[j]], \
                                          formDC[[j, jj]], \
                                          potentialdimlessnumbers[[j, jj, k]], -1}}];
                                        Return[solution];
                                        ,
                                        solution = Join[solution, {{namesDC[[j]], \
                                          formDC[[j, jj]], \
                                          potentialdimlessnumbers[[j, jj, k]], -1}}];
                                        Return[solution];
                                    ];
                                ];
                            ];
                        ];
                    ];
                    
                ];
                
                If[flag1 == 0,
                    Print["CRITICAL ERROR: dimlessnumbertryer: Did not identify the dimless number and the scales associated. Term is: ", solutioninput[[i, 1]], " and " solutioninput[[i, 2]]];
                    Return[Null];
                    ,
                    solution = 0;
                    Break[];
                ];
                
            ];
        ];
                
        If[chosen == 0,
            If[functioninput[[coords3[[1]], coords3[[2]], coords3[[3]]]] == listProduct[dummychecksolutionvec],
                solution = {};
                ,
                solution = 0;
                ,
                solution = 0;
            ];
        ];
           
        Return[solution];
    ];


predecider[term_, scalestocheck_, symbolsforscales_, namesDC_, formDC_, dummyS_] :=
    Block[{solution},
        If[Head[term] == List,
            solution = caseListpredecider[term, scalestocheck, symbolsforscales, namesDC, \
              formDC, dummyS];
            ,
            solution = caseNonListpredecider[term, scalestocheck, symbolsforscales, namesDC, \
              formDC, dummyS];
            ,
            solution = caseNonListpredecider[term, scalestocheck, symbolsforscales, namesDC, \
              formDC, dummyS];
        ];
        
        Return[solution];
    ];


caseListpredecider[term_, scalestocheck_, symbolsforscales_, namesDC_, formDC_, dummyS_] :=
    Block[{i, solution = ConstantArray[0, Length[term]]},
        
        coords4 = Append[coords4, 1];
        
        For[i = 1, i <= Length[term], i++,
            coords4[[-1]] = i;
            solution[[i]] = predecider[term[[i]], scalestocheck, symbolsforscales, namesDC, \
              formDC, dummyS];
        ];
        
        coords4 = Drop[coords4, -1];
        
        Return[solution];
    ];


caseNonListpredecider[term_, scalestocheck_, symbolsforscales_, namesDC_, formDC_, dummyS_] :=
    Block[{solution, levelcountdummy = 0, i, symbolsforscalescount},
        
        symbolsforscalescount = ConstantArray[1, Length[symbolsforscales]];
        
        solution = decider[term, scalestocheck, symbolsforscales, namesDC, formDC, dummyS, \
          levelcountdummy];
        
        symbolsforscalescountglobal[[coords4[[1]], coords4[[2]], coords4[[3]]]] = \
          symbolsforscalescount - 1;
        
        Return[solution];
    ];


decider[term_, scalestocheck_, symbolsforscales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{dimlesscoef = 1, levelcount = levelcountdummy + 1, chosen = 0},
        
        printouttoggle = 0;
                
        If[printouttoggle == 1,
            Print[term];                
        ];
        If[levelcount == 10,
            
            Print["Reached levelcount limit. Backing out."];,
            
            If[Head[term] == Rational,
                If[printouttoggle == 1,
                    Print["decider: Identified Rational"];                
                ];
                chosen = 1;
                dimlesscoef = caseRational[term, scalestocheck, symbolsforscales, \
                  namesDC, formDC, dummyS, levelcountdummy];
            ];
            If[Head[term] == Symbol,
                If[printouttoggle == 1,
                    Print["decider: Identified Symbol"];                
                ];
                chosen = 1;
                dimlesscoef = caseSymbol[term, scalestocheck, symbolsforscales, \
                  namesDC, formDC, dummyS, levelcountdummy];
            ];
            If[Head[term] == Integer,
                If[printouttoggle == 1,
                    Print["decider: Identified Integer"];                
                ];
                chosen = 1;
                dimlesscoef = caseInteger[term, scalestocheck, symbolsforscales, \
                  namesDC, formDC, dummyS, levelcountdummy];
            ];
            If[Head[term] == Power,
                If[printouttoggle == 1,
                    Print["decider: Identified Power"];                
                ];
                chosen = 1;
                dimlesscoef = casePower[term, scalestocheck, symbolsforscales, \
                  namesDC, formDC, dummyS, levelcountdummy];
            ];
            If[Head[term] == List,
                If[printouttoggle == 1,
                    Print["decider: Identified List"];                
                ];
                chosen = 1;
                dimlesscoef = caseList[term, scalestocheck, symbolsforscales, \
                  namesDC, formDC, dummyS, levelcountdummy];
            ];
            If[Head[term] == Times,
                If[printouttoggle == 1,
                    Print["decider: Identified Times"];                
                ];
                chosen = 1;
                dimlesscoef = caseTimes[term, scalestocheck, symbolsforscales, \
                  namesDC, formDC, dummyS, levelcountdummy];
            ];
            If[chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified None"];                
                ];
                Print["CRITICAL ERROR: decider: Did not identify the Head of the term."];
                Return[Null];
            ];
            
            ,
            
            If[printouttoggle == 1,
                Print["CRITICAL ERROR: decider: Problems."];                
            ];
            Return[Null];
            
        ];
        
        If[printouttoggle == 1,
            Print[dimlesscoef];
        ];
        
        Return[dimlesscoef];
        
    ];


caseList[term_, scalestocheck_, symbolsforscales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{},
        Print["CRITICAL ERROR: caseList: This case should not happen, since we have the predecider."];
        Return[Null];
    ];


caseRational[term_, scalestocheck_, symbolsforscales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{dimlesscoef = term},
        Return[dimlesscoef];
    ];


caseSymbol[term_, scalestocheck_, symbolsforscales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{dimlesscoef = 1, flag = 0, i, j},
        For[i = 1, i <= Length[scalestocheck], i++,
            For[j = 1, j <= Length[scalestocheck[[i]]], j++,
                If[scalestocheck[[i, j]] == term,
                    flag = 1;
                    dimlesscoef = Symbol[ToString[symbolsforscales[[i]]] <> ToString[ \
                      symbolsforscalescount[[i]]]];
                    symbolsforscalescount[[i]] += 1;
                    Break[];
                ];
            ];
            If[flag == 1,
                Break[];
            ];
        ];
        
        If[flag == 0,
            Print["CRITICAL ERROR: caseSymbol: Evaluated a Symbol that was not given in the set of scales. Scales are: ", scalestocheck, ". The symbol was: ", term];
            Return[Null];
            ,
            flag = 0;
        ];
        
        Return[dimlesscoef];
    ];


caseInteger[term_, scalestocheck_, symbolsforscales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{dimlesscoef = 1},
        If[term == 1 || term == -1,
            dimlesscoef = term;
            Return[dimlesscoef];
            ,
            dimlesscoef = term;
            If[term == 0,
                ,
                Print["caseInteger: Part of term's scale is an integer that is not 1 or -1."];
                ,
                Print["caseInteger: Part of term's scale is an integer that is not 1 or -1."];
            ];
            Return[dimlesscoef];
            ,
            Print["CRITICAL ERROR: caseInteger: Unidentified integer."];
            Return[Null];
        ];
    ];


casePower[term_, scalestocheck_, symbolsforscales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{dimlesscoef = 1, power},
        power = term[[2]];
        dimlesscoef = decider[term[[1]], scalestocheck, symbolsforscales, namesDC, \
          formDC, dummyS, levelcountdummy]^power;
        
        Return[dimlesscoef];
    ];


caseTimes[term_, scalestocheck_, symbolsforscales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{dimlesscoef = 1, i, scalesvec},
        scalesvec = ConstantArray[0, Length[term]];
        For[i = 1, i <= Length[term], i++,
            scalesvec[[i]] = decider[term[[i]], scalestocheck, symbolsforscales, \
              namesDC, formDC, dummyS, levelcountdummy];
        ];
        dimlesscoef = listProduct[scalesvec];
        
        Return[dimlesscoef];
    ];


casePlus[term_, scalestocheck_, symbolsforscales_, namesDC_, formDC_, dummyS_, \
levelcountdummy_] :=
    Block[{},
        Print["CRITICAL ERROR: casePlus: Not written yet."];
        Return[Null];
    ];


listProduct[x_List] :=
    Times @@ x;


End[]

EndPackage[]

