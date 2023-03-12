(* ::Package:: *)

(* Symbolica \[Copyright] 2023 by Kyle Pietrzyk is licensed under Attribution-NonCommercial-ShareAlike 4.0 International *)


(****** ApplyBoundaryConditions.wl -- 
*************)

(* 

ApplyBoundaryConditions[term, ] 

*)


(* Created November 26 2020
Modified June 14 2020: 
                       *)



BeginPackage["Disruptioneering`ApplyBoundaryConditions`"];

ApplyBoundaryConditions::usage = 
"ApplyBoundaryConditions[term, ]";



Begin["`Disruptioneering`"];

(*avgdenoters should be {{Subscript[\[LeftAngleBracket]spaceholder\[RightAngleBracket], sub], sub, spaceholder},
{{{\[CapitalOmega],\[CapitalGamma]},{\[Xi],\[Eta]},{{\[CapitalOmega]a,\[CapitalOmega]b},{\[CapitalGamma]a,\[CapitalGamma]b}}},{{\[CapitalOmega]a,\[CapitalGamma]a},{\[Xi], \[Eta]},{{},{\[CapitalGamma]ai,\[CapitalGamma]ao}}},
{{\[CapitalOmega]b,\[CapitalGamma]b},{\[Xi], \[Eta]},{{},{\[CapitalGamma]bi,\[CapitalGamma]bo}}}{{\[CapitalDelta]\[Tau]PE1,\[Delta]\[Tau]PE1}, {\[Tau]Pe1},{{},{}}}}}*)
(*graddenoters should be {Grad[spaceholder, sub], sub, spaceholder}*)
(*divdenoters should be {Div[spaceholder, sub], sub, spaceholder}*)

ApplyBoundaryConditions[term_, boundarycondition_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, \
normalvecform_, Uform_, slowIV_, fastIV_, option_, option2_] := 
    procedure[term, boundarycondition, scalars, vectors, tensors, closurevar, graddenoters, \
      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
      divform, gradform, normalvecform, Uform, slowIV, fastIV, option, option2, \
      ConstantArray[{}, Length[boundarycondition]]];

ApplyBoundaryConditions[term_, boundarycondition_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, \
normalvecform_, Uform_, slowIV_, fastIV_, option_, option2_, option3_] := 
    procedure[term, boundarycondition, scalars, vectors, tensors, closurevar, graddenoters, \
      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
      divform, gradform, normalvecform, Uform, slowIV, fastIV, option, option2, option3];


procedure[term_, boundarycondition_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, option_, \
option2_, option3_] :=
    Block[{solution, uvecdependency, newvectors, i, isubin, levelcountdummy = 0, lookforinfo, \
    subininfo, symbol, GlobalAnticipateGroupingSmallVariableTermsForDoubleDot, looking, \
    lookforinfoglobalexpandedtotal, lookingfirsttime, lookforinfoglobalexpandeddummy, \
    lookforinfoglobalexpandeddummyN, iadded, icollected, flag, icombine},
        
        newvectors = vectors;
        
        GlobalAnticipateGroupingSmallVariableTermsForDoubleDot = "On";
        
        
        
        solution = term;
        For[isubin = 1, isubin <= Length[boundarycondition], isubin++,
            looking = 1;
            lookingfirsttime = 1;
            lookforinfo = getterminfo[Expand[boundarycondition[[isubin, 1]]], {}, scalars, \
              newvectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
              dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, levelcountdummy, {}];
            looking = 0;
            lookingfirsttime = 0;
            lookforinfoglobalexpandedtotal = lookforinfo;
            lookforinfo = MakeSingleTerm[lookforinfo, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            
            
            lookforinfoglobalexpandeddummy = {lookforinfoglobalexpandedtotal[[1]]};
            lookforinfoglobalexpandeddummyN = {1};
            For[iadded = 2, iadded <= Length[lookforinfoglobalexpandedtotal], iadded++,
                flag = 0;
                For[icollected = 1, icollected <= Length[lookforinfoglobalexpandeddummy], icollected++,
                    If[checkListInList[lookforinfoglobalexpandeddummy[[icollected]], lookforinfoglobalexpandedtotal[[iadded]]],
                        flag = 1;
                        lookforinfoglobalexpandeddummyN[[icollected]] += 1;
                        Break[];
                    ];
                ];
                If[flag == 0,
                    lookforinfoglobalexpandeddummy = Join[lookforinfoglobalexpandeddummy, {lookforinfoglobalexpandedtotal[[iadded]]}];
                    lookforinfoglobalexpandeddummyN = Join[lookforinfoglobalexpandeddummyN, {1}];
                ];
            ];
            For[icombine = 1, icombine <= Length[lookforinfoglobalexpandeddummy], icombine++,
                If[lookforinfoglobalexpandeddummyN[[icombine]] > 1,
                    lookforinfoglobalexpandeddummy[[icombine, 1]] = Join[{lookforinfoglobalexpandeddummyN[[icombine]]}, lookforinfoglobalexpandeddummy[[icombine, 1]]];
                    lookforinfoglobalexpandeddummy[[icombine, 2]] = Join[{0}, lookforinfoglobalexpandeddummy[[icombine, 2]]];
                    lookforinfoglobalexpandeddummy[[icombine, 3]] = Join[{0}, lookforinfoglobalexpandeddummy[[icombine, 3]]];
                ];
            ];
            lookforinfoglobalexpandedtotal = lookforinfoglobalexpandeddummy;
            
            
            subininfo = getterminfo[Expand[boundarycondition[[isubin, 2]]], {}, scalars, \
              newvectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
              dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, levelcountdummy, {}];
            subininfo = MakeSingleTerm[subininfo, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            If[subininfo == {{{0}, {0}, {0}}},
                subininfo = {{{0}, {lookforinfo[[1, 2, 1]]}, {0}}};
            ];
            
            If[lookforinfo[[1, 2, 1]] == subininfo[[1, 2, 1]],
                solution = main[Expand[solution], Expand[boundarycondition[[isubin]]], \
                  scalars, newvectors, tensors, closurevar, graddenoters, divdenoters, \
                  avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, 1, option2, option3[[isubin]]];
                ,
                Print["CRITICAL ERROR: procedure: The provided term to look for and the term to sub in do not have the same tensor rank. The pair is: ", boundarycondition[[isubin]]];
                Return[Null];
            ];
        ];
        
        solution = Expand[solution];
        
        Return[solution];
    ];


main[term_, subin_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, \
avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, \
normalvecform_, Uform_, slowIV_, fastIV_, option_, option2_, option3_] :=
    Block[{solution},
        
        If[Head[term] == List,
            solution = caseListmain[term, subin, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, option, option2, option3];
            ,
            solution = caseNonListmain[term, subin, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, option, option2, option3];
            ,
            solution = caseNonListmain[term, subin, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, option, option2, option3];
        ];
        
        Return[solution];
    ];


caseListmain[term_, subin_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, \
normalvecform_, Uform_, slowIV_, fastIV_, option_, option2_, option3_] :=
    Block[{i, solution = ConstantArray[0, Length[Expand[term]]], toplayer = option, chosen = 0,
    successfullyfound, ilookfor, lookfor, sub, lookforinfo, levelcountdummy = 0, newscalars, \
    newvectors, newtensors, ieqsection, iaddedterms, multipliedparts, solutionaddedterms, \
    option2global, lookforglobal = Null, potentialmultipliedparts, chosen2 = 0, iinstance, \
    successmults, sectionmults, itermmults, isuccessmults, expandedterm, looking = 0, \
    expandedtermfound, successfullyfound2, dummysuccessfullyfound2, ioperator, \
    potentialmultipliedpartssolution, successsolution, sectionsolution, successmultsindex, \
    successsolutionindex, icheckmult, ichecksolution, isubin, isuccesssolution, \
    itermsolution, submults, subinsolution, suboutsolution, solutiondummy, ichecksoleq, \
    ichecksolsec, itermcases, icheckmultcases, flag, ichecksolutioncases, newsuccessmultsindex, \
    dummysuccessmultsindex, icombo, newcombo, dummysuccesssolutionindex, newsuccesssolutionindex, \
    finalindices, successfullyfoundneg, successfullyfoundneg2, dummysuccessfullyfoundneg2, \
    lookforinfoglobalexpanded},
        
        expandedterm = Expand[term];
        
        (*Note: toplayer is the equation layer*)
        
        For[i = 1, i <= Length[expandedterm], i++,
            If[toplayer == 1,
                chosen = 0;
                lookfor = subin[[1]];
                sub = subin[[2]];
                
                If[Head[lookfor] == Plus,
                    chosen = 1;
                    successfullyfound = ConstantArray[0, Length[lookfor]];
                    successfullyfoundneg = ConstantArray[0, Length[lookfor]];
                    expandedtermfound = ConstantArray[0, Length[lookfor]];
                    potentialmultipliedparts = ConstantArray[0, Length[lookfor]];
                    potentialmultipliedpartssolution = ConstantArray[0, Length[lookfor]];
                    For[ilookfor = 1, ilookfor <= Length[lookfor], ilookfor++,
                        
                        newscalars = scalars;
                        newvectors = vectors;
                        newtensors = tensors;
                        
                        looking = 0;
                        lookforinfo = getterminfo[lookfor[[ilookfor]], {}, scalars, vectors, \
                          tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                          dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                          gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, {}];
                        lookforinfo = MakeSingleTerm[lookforinfo, {}, dotdenoters, \
                          tensorproductdenoters, doubledotdenoters];
                        lookforinfoglobalexpanded = {lookforinfoglobalexpandedtotal[[ilookfor]]};
                        
                        If[lookforinfo[[1, 2, 1]] == 0,
                            newscalars = Join[newscalars, {option2}];
                        ];
                        If[lookforinfo[[1, 2, 1]] == 1,
                            newvectors = Join[newvectors, {option2}];
                        ];
                        If[lookforinfo[[1, 2, 1]] == 2,
                            newtensors = Join[newtensors, {option2}];
                        ];
                        
                        looking = 1;
                        successfullyfound[[ilookfor]] = main[expandedterm[[i]], \
                          {lookforinfo, lookforinfoglobalexpanded, looking}, newscalars, newvectors, newtensors, \
                          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                          normalvecform, Uform, slowIV, fastIV, 0, option2, option3];
                        successfullyfound[[ilookfor]] = Expand[successfullyfound[[ilookfor]]];
                        successfullyfoundneg[[ilookfor]] = main[-expandedterm[[i]], \
                          {lookforinfo, lookforinfoglobalexpanded, looking}, newscalars, newvectors, newtensors, \
                          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                          normalvecform, Uform, slowIV, fastIV, 0, option2, option3];
                        successfullyfoundneg[[ilookfor]] = Expand[successfullyfoundneg[[ilookfor]]];
                        looking = 0;
                        expandedtermfound[[ilookfor]] = main[expandedterm[[i]], \
                          {lookforinfo, lookforinfoglobalexpanded, looking}, newscalars, newvectors, newtensors, \
                          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                          normalvecform, Uform, slowIV, fastIV, 0, option2, option3];
                        expandedtermfound[[ilookfor]] = Expand[expandedtermfound[[ilookfor]]];
                        
                        
                        solution[[i]] = {};
                        potentialmultipliedparts[[ilookfor]] = ConstantArray[0, Length[ \
                          successfullyfound[[ilookfor]]]];
                        potentialmultipliedpartssolution[[ilookfor]] = ConstantArray[0, \
                          Length[successfullyfound[[ilookfor]]]];
                        For[ieqsection = 1, ieqsection <= Length[ \
                          successfullyfound[[ilookfor]]], ieqsection++,
                            
                            potentialmultipliedparts[[ilookfor, ieqsection]] = {};
                            potentialmultipliedpartssolution[[ilookfor, ieqsection]] = {};
                            solutionaddedterms = ConstantArray[0, Length[ \
                              successfullyfound[[ilookfor, ieqsection]]]];
                            
                            (*Check successfullyfound to see if the solution was identified*)
                            For[iaddedterms = 1, iaddedterms <= Length[ \
                              successfullyfound[[ilookfor, ieqsection]]], iaddedterms++,
                                
                                chosen2 = 0;
                                potentialmultipliedparts[[ilookfor, ieqsection]] = Join[ \
                                  potentialmultipliedparts[[ilookfor, ieqsection]], {{}}];
                                potentialmultipliedpartssolution[[ilookfor, ieqsection]] = \
                                  Join[potentialmultipliedpartssolution[[ilookfor, \
                                  ieqsection]], {{}}];
                                successfullyfound2 = successfullyfound[[ilookfor, ieqsection, \
                                  iaddedterms, 1]];
                                
                                
                                For[ioperator = 1, ioperator <= Length[successfullyfound2], \
                                  ioperator++,
                                    
                                    dummysuccessfullyfound2 = successfullyfound2[[ioperator]] \
                                     /. option2 -> option2^2;
                                    
                                    If[successfullyfound2[[ioperator]] == \
                                      dummysuccessfullyfound2,
                                        ,
                                        chosen2 = 1;
                                        multipliedparts = listProduct[Delete[ \
                                          successfullyfound2, ioperator] /. {option2 -> \
                                          lookfor[[ilookfor]]}];
                                        potentialmultipliedparts[[ilookfor, ieqsection, -1]] = \
                                          Join[potentialmultipliedparts[[ilookfor, \
                                          ieqsection, -1]], {multipliedparts}];
                                        potentialmultipliedpartssolution[[ilookfor, ieqsection, \
                                          -1]] = Join[potentialmultipliedpartssolution[[ \
                                          ilookfor, ieqsection, -1]], {successfullyfound2[[ \
                                          ioperator]]}];
                                        solutionaddedterms[[iaddedterms]] = Expand[ \
                                          multipliedparts * successfullyfound2[[ioperator]] /. \
                                          {option2 -> lookfor[[ilookfor]]}];
                                        ,
                                        chosen2 = 1;
                                        multipliedparts = listProduct[Delete[ \
                                          successfullyfound2, ioperator] /. {option2 -> \
                                          lookfor[[ilookfor]]}];
                                        potentialmultipliedparts[[ilookfor, ieqsection, -1]] = \
                                          Join[potentialmultipliedparts[[ilookfor, \
                                          ieqsection, -1]], {multipliedparts}];
                                        potentialmultipliedpartssolution[[ilookfor, ieqsection, \
                                          -1]] = Join[potentialmultipliedpartssolution[[ \
                                          ilookfor, ieqsection, -1]], {successfullyfound2[[ \
                                          ioperator]]}];
                                        solutionaddedterms[[iaddedterms]] = Expand[ \
                                          multipliedparts * successfullyfound2[[ioperator]] /. \
                                          {option2 -> lookfor[[ilookfor]]}];
                                    ];
                                ];
                                
                                
                                If[chosen2 == 0,
                                    solutionaddedterms[[iaddedterms]] = listProduct[ \
                                      successfullyfound[[ilookfor, ieqsection, iaddedterms, \
                                      1]]] /. {option2 -> lookfor[[ilookfor]]};
                                ];
                            ];
                            
                            
                            (*If no term was identified in this section of the equation,
                            try successfullyfoundneg to see if terms were identified*)
                            If[Flatten[potentialmultipliedparts[[ilookfor, ieqsection]]] == {} && \
                              Flatten[potentialmultipliedpartssolution[[ilookfor, ieqsection]]] == {},
                                
                                potentialmultipliedparts[[ilookfor, ieqsection]] = {};
                                potentialmultipliedpartssolution[[ilookfor, ieqsection]] = {};
                                
                                For[iaddedterms = 1, iaddedterms <= Length[ \
                                  successfullyfoundneg[[ilookfor, ieqsection]]], iaddedterms++,
                                    
                                    potentialmultipliedparts[[ilookfor, ieqsection]] = Join[ \
                                      potentialmultipliedparts[[ilookfor, ieqsection]], {{}}];
                                    potentialmultipliedpartssolution[[ilookfor, ieqsection]] = \
                                      Join[potentialmultipliedpartssolution[[ilookfor, \
                                      ieqsection]], {{}}];
                                    successfullyfoundneg2 = successfullyfoundneg[[ilookfor, \
                                      ieqsection, iaddedterms, 1]];
                                    
                                    
                                    For[ioperator = 1, ioperator <= Length[ \
                                      successfullyfoundneg2], ioperator++,
                                        
                                        dummysuccessfullyfound2 = successfullyfoundneg2[[ \
                                          ioperator]] /. option2 -> option2^2;
                                        
                                        If[successfullyfoundneg2[[ioperator]] == \
                                          dummysuccessfullyfound2,
                                            ,
                                            multipliedparts = -listProduct[Delete[ \
                                              successfullyfoundneg2, ioperator] /. {option2 -> \
                                              lookfor[[ilookfor]]}];
                                            potentialmultipliedparts[[ilookfor, ieqsection, \
                                              -1]] = Join[potentialmultipliedparts[[ilookfor, \
                                              ieqsection, -1]], {multipliedparts}];
                                            potentialmultipliedpartssolution[[ilookfor, \
                                              ieqsection, -1]] = Join[ \
                                              potentialmultipliedpartssolution[[ilookfor, \
                                              ieqsection, -1]], {successfullyfoundneg2[[ \
                                              ioperator]]}];
                                            ,
                                            multipliedparts = -listProduct[Delete[ \
                                              successfullyfoundneg2, ioperator] /. {option2 -> \
                                              lookfor[[ilookfor]]}];
                                            potentialmultipliedparts[[ilookfor, ieqsection, \
                                              -1]] = Join[potentialmultipliedparts[[ilookfor, \
                                              ieqsection, -1]], {multipliedparts}];
                                            potentialmultipliedpartssolution[[ilookfor, \
                                              ieqsection, -1]] = Join[ \
                                              potentialmultipliedpartssolution[[ilookfor, \
                                              ieqsection, -1]], {successfullyfoundneg2[[ \
                                              ioperator]]}];
                                        ];
                                    ];
                                    
                                ];
                                
                            ];
                            
                            
                            solution[[i]] = Expand[Join[solution[[i]], \
                              {Total[solutionaddedterms]}]];
                        ];
                        
                    ];
                    
                    
                    For[ieqsection = 1, ieqsection <= Length[potentialmultipliedparts[[1]]], \
                      ieqsection++,
                        
                        sectionmults = potentialmultipliedparts[[;;, ieqsection]];
                        successmults = sectionmults[[1]];
                        successmultsindex = {};
                        dummysuccessmultsindex = {};
                        sectionsolution = potentialmultipliedpartssolution[[;;, ieqsection]];
                        successsolution = sectionsolution[[1]];
                        successsolutionindex = {};
                        dummysuccesssolutionindex = {};
                        
                        If[option3 == {},
                            (*Check for similar multiples*)
                            For[ilookfor = 2, ilookfor <= Length[sectionmults], ilookfor++,
                                For[iaddedterms = 1, iaddedterms <= Length[successmults], iaddedterms++,
                                    successmults[[iaddedterms]] = Intersection[successmults[[iaddedterms]], \
                                      Flatten[sectionmults[[ilookfor]]]];
                                ];
                            ];
                            ,
                            (*Check multiples in the option3 vector*)
                            For[ilookfor = 2, ilookfor <= Length[sectionmults], ilookfor++,
                                For[iaddedterms = 1, iaddedterms <= Length[successmults], iaddedterms++,
                                    successmults[[iaddedterms]] = Intersection[successmults[[iaddedterms]] / option3[[1]], \
                                      Flatten[sectionmults[[ilookfor]]] / option3[[ilookfor]]];
                                ];
                            ];
                        ];
                        
                        If[successmults != {},
                            For[isuccessmults = 1, isuccessmults <= Length[successmults], isuccessmults++,
                                For[itermcases = 1, itermcases <= Length[successmults[[isuccessmults]]], itermcases++,
                                    dummysuccessmultsindex = Join[dummysuccessmultsindex, {{{isuccessmults, itermcases}}}];
                                    For[ilookfor = 2, ilookfor <= Length[sectionmults], ilookfor++,
                                        newsuccessmultsindex = {};
                                        For[icombo = 1, icombo <= Length[dummysuccessmultsindex], icombo++,
                                            For[icheckmult = 1, icheckmult <= Length[sectionmults[[ilookfor]]], icheckmult++,
                                                If[Intersection[Flatten[dummysuccessmultsindex[[icombo, ;;, 1]]], {icheckmult}] == {},
                                                    For[icheckmultcases = 1, icheckmultcases <= Length[sectionmults[[ilookfor, icheckmult]]], icheckmultcases++,
                                                        If[successmults[[isuccessmults, itermcases]] == sectionmults[[ilookfor, icheckmult, icheckmultcases]],
                                                            newcombo = Join[dummysuccessmultsindex[[icombo]], {{icheckmult, icheckmultcases}}];
                                                            newsuccessmultsindex = Join[newsuccessmultsindex, {newcombo}];
                                                        ];
                                                    ];
                                                ];
                                            ];
                                        ];
                                        dummysuccessmultsindex = newsuccessmultsindex;
                                    ];
                                    If[Length[dummysuccessmultsindex] == 0,
                                        dummysuccessmultsindex = {};
                                        ,
                                        successmultsindex = Join[successmultsindex, dummysuccessmultsindex];
                                        dummysuccessmultsindex = {};
                                    ];
                                ];
                            ];
                        ];
                        
                        
                        (*Check for similar SUBDUMMY terms*)
                        For[ilookfor = 2, ilookfor <= Length[sectionsolution], ilookfor++,
                            For[iaddedterms = 1, iaddedterms <= Length[successsolution], iaddedterms++,
                                successsolution[[iaddedterms]] = Intersection[successsolution[[iaddedterms]], \
                                  Flatten[sectionsolution[[ilookfor]]]];
                            ];
                        ];
                        
                        If[successsolution != {},
                            For[isuccesssolution = 1, isuccesssolution <= Length[successsolution], isuccesssolution++,
                                For[itermcases = 1, itermcases <= Length[successsolution[[isuccesssolution]]], itermcases++,
                                    dummysuccesssolutionindex = Join[dummysuccesssolutionindex, {{{isuccesssolution, itermcases}}}];
                                    For[ilookfor = 2, ilookfor <= Length[sectionsolution], ilookfor++,
                                        newsuccesssolutionindex = {};
                                        For[icombo = 1, icombo <= Length[dummysuccesssolutionindex], icombo++,
                                            For[ichecksolution = 1, ichecksolution <= Length[sectionsolution[[ilookfor]]], ichecksolution++,
                                                If[Intersection[Flatten[dummysuccesssolutionindex[[icombo, ;;, 1]]], {ichecksolution}] == {},
                                                    For[ichecksolutioncases = 1, ichecksolutioncases <= Length[sectionsolution[[ilookfor, ichecksolution]]], ichecksolutioncases++,
                                                        If[successsolution[[isuccesssolution, itermcases]] == sectionsolution[[ilookfor, ichecksolution, ichecksolutioncases]],
                                                            newcombo = Join[dummysuccesssolutionindex[[icombo]], {{ichecksolution, ichecksolutioncases}}];
                                                            newsuccesssolutionindex = Join[newsuccesssolutionindex, {newcombo}];
                                                        ];
                                                    ];
                                                ];
                                            ];
                                        ];
                                        dummysuccesssolutionindex = newsuccesssolutionindex;
                                    ];
                                    If[Length[dummysuccesssolutionindex] == 0,
                                        dummysuccesssolutionindex = {};
                                        ,
                                        successsolutionindex = Join[successsolutionindex, dummysuccesssolutionindex];
                                        dummysuccesssolutionindex = {};
                                    ];
                                ];
                            ];
                        ];
                        
                        
                        
                        (*Check that multiples' index and SUBDUMMY terms index matches*)
                        finalindices = {{{0, 0}}};
                        For[itermmults = 1, itermmults <= Length[successmultsindex], itermmults++,
                            If[Intersection[Flatten[finalindices[[;;, ;;, 1]]], \
                              successmultsindex[[itermmults, ;;, 1]]] == {},
                                For[itermsolution = 1, itermsolution <= Length[successsolutionindex], \
                                  itermsolution++,
                                    If[Intersection[Flatten[finalindices[[;;, ;;, 1]]], \
                                      successsolutionindex[[itermsolution, ;;, 1]]] == {},
                                        If[successmultsindex[[itermmults]] == \
                                          successsolutionindex[[itermsolution]],
                                            If[finalindices == {{{0, 0}}},
                                                finalindices = {successmultsindex[[itermmults]]};
                                                ,
                                                finalindices = Join[finalindices, {successmultsindex[[itermmults]]}];
                                            ];
                                        ];
                                    ];
                                ];
                            ];
                        ];
                        
                        
                        (*Finally, subin and subout the terms that need to be according to
                        the combinations in finalindices*)
                        If[finalindices != {{{0, 0}}},
                            For[icombo = 1, icombo <= Length[finalindices], icombo++,
                                submults = sectionmults[[1, finalindices[[icombo, 1, 1]], \
                                  finalindices[[icombo, 1, 2]]]];
                                subinsolution = 0;
                                If[Head[sub] == Plus,
                                    For[isubin = 1, isubin <= Length[sub], isubin++,
                                      subinsolution += \
                                        sectionsolution[[1, finalindices[[icombo, 1, 1]], \
                                        finalindices[[icombo, 1, 2]]]] /. {option2 -> \
                                        sub[[isubin]]};
                                    ];
                                    ,
                                    subinsolution = \
                                      sectionsolution[[1, finalindices[[icombo, 1, 1]], \
                                      finalindices[[icombo, 1, 2]]]] /. {option2 -> sub};
                                    ,
                                    subinsolution = \
                                      sectionsolution[[1, finalindices[[icombo, 1, 1]], \
                                      finalindices[[icombo, 1, 2]]]] /. {option2 -> sub};
                                ];
                                
                                suboutsolution = 0;
                                If[Head[lookfor] == Plus,
                                    If[option3 == {},
                                        For[ilookfor = 1, ilookfor <= Length[lookfor], ilookfor++,
                                            suboutsolution += \
                                              sectionsolution[[1, finalindices[[icombo, 1, 1]], \
                                              finalindices[[icombo, 1, 2]]]] /. {option2 -> \
                                              lookfor[[ilookfor]]};
                                        ];
                                        ,
                                        For[ilookfor = 1, ilookfor <= Length[lookfor], ilookfor++,
                                            suboutsolution += \
                                              sectionsolution[[1, finalindices[[icombo, 1, 1]], \
                                              finalindices[[icombo, 1, 2]]]] * \
                                              option3[[ilookfor]] /. {option2 -> \
                                              lookfor[[ilookfor]]};
                                        ];
                                    ];
                                    ,
                                    If[option3 == {},
                                        suboutsolution = \
                                          sectionsolution[[1, finalindices[[icombo, 1, 1]], \
                                          finalindices[[icombo, 1, 2]]]] /. {option2 -> lookfor};
                                        ,
                                        suboutsolution = \
                                          sectionsolution[[1, finalindices[[icombo, 1, 1]], \
                                          finalindices[[icombo, 1, 2]]]] * option3[[1]] /. \
                                          {option2 -> lookfor};
                                    ];
                                    ,
                                    If[option3 == {},
                                        suboutsolution = \
                                          sectionsolution[[1, finalindices[[icombo, 1, 1]], \
                                          finalindices[[icombo, 1, 2]]]] /. {option2 -> lookfor};
                                        ,
                                        suboutsolution = \
                                          sectionsolution[[1, finalindices[[icombo, 1, 1]], \
                                          finalindices[[icombo, 1, 2]]]] * option3[[1]] /. \
                                          {option2 -> lookfor};
                                    ];
                                ];
                                
                                
                                solution[[i, ieqsection]] = Expand[ \
                                  solution[[i, ieqsection]] + \
                                  (submults * subinsolution) - \
                                  (submults * suboutsolution)];
                            ];
                        ];
                        
                    ];
                    
                    
                ];
                
                
                
                
                If[chosen == 0,
                    newscalars = scalars;
                    newvectors = vectors;
                    newtensors = tensors;
                    
                    looking = 0;
                    lookforinfo = getterminfo[lookfor, {}, scalars, vectors, \
                      tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                      dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                      normalvecform, Uform, slowIV, fastIV, levelcountdummy, {}];
                    lookforinfo = MakeSingleTerm[lookforinfo, {}, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters];
                    lookforinfoglobalexpanded = lookforinfoglobalexpandedtotal;
                    
                    If[lookforinfo[[1, 2, 1]] == 0,
                        newscalars = Join[newscalars, {option2}];
                    ];
                    If[lookforinfo[[1, 2, 1]] == 1,
                        newvectors = Join[newvectors, {option2}];
                    ];
                    If[lookforinfo[[1, 2, 1]] == 2,
                        newtensors = Join[newtensors, {option2}];
                    ];
                    
                    
                    looking = 1;
                    successfullyfound = main[term[[i]], {lookforinfo, lookforinfoglobalexpanded, looking}, newscalars, \
                      newvectors, newtensors, closurevar, graddenoters, divdenoters, \
                      avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                      normalvecform, Uform, slowIV, fastIV, 0, option2, option3];
                    successfullyfound = Expand[successfullyfound];
                    successfullyfoundneg = main[-term[[i]], {lookforinfo, lookforinfoglobalexpanded, looking}, \
                      newscalars, newvectors, newtensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                      gradform, normalvecform, Uform, slowIV, fastIV, 0, option2, option3];
                    successfullyfoundneg = Expand[successfullyfoundneg];
                    looking = 0;
                    expandedtermfound = main[expandedterm[[i]], {lookforinfo, lookforinfoglobalexpanded, looking}, \
                      newscalars, newvectors, newtensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                      gradform, normalvecform, Uform, slowIV, fastIV, 0, option2, option3];
                    expandedtermfound = Expand[expandedtermfound];
                    
                    
                    solution[[i]] = {};
                    For[ieqsection = 1, ieqsection <= Length[successfullyfound], ieqsection++,
                        
                        solutionaddedterms = ConstantArray[0, Length[ \
                          successfullyfound[[ieqsection]]]];
                        
                        For[iaddedterms = 1, iaddedterms <= Length[ \
                          successfullyfound[[ieqsection]]], iaddedterms++,
                            
                            chosen2 = 0;
                            successfullyfound2 = successfullyfound[[ieqsection, \
                              iaddedterms, 1]];
                            successfullyfoundneg2 = successfullyfoundneg[[ieqsection, \
                              iaddedterms, 1]];
                            
                            
                            For[ioperator = 1, ioperator <= Length[successfullyfound2], \
                              ioperator++,
                              
                                dummysuccessfullyfound2 = successfullyfound2[[ioperator]] \
                                 /. option2 -> option2^2;
                                
                                If[successfullyfound2[[ioperator]] == \
                                  dummysuccessfullyfound2,
                                    ,
                                    chosen2 = 1;
                                    solutionaddedterms[[iaddedterms]] = listProduct[ \
                                      successfullyfound2 /. {option2 -> sub}];
                                    Break[];
                                    ,
                                    chosen2 = 1;
                                    solutionaddedterms[[iaddedterms]] = listProduct[ \
                                      successfullyfound2 /. {option2 -> sub}];
                                    Break[];
                                ];
                            ];
                            
                            
                            
                            If[chosen2 == 0,
                                For[ioperator = 1, ioperator <= Length[successfullyfoundneg2], \
                                  ioperator++,
                                  
                                    dummysuccessfullyfoundneg2 = successfullyfoundneg2[[ioperator]] \
                                     /. option2 -> option2^2;
                                    
                                    If[successfullyfoundneg2[[ioperator]] == \
                                      dummysuccessfullyfoundneg2,
                                        ,
                                        chosen2 = 1;
                                        solutionaddedterms[[iaddedterms]] = listProduct[ \
                                          successfullyfoundneg2 /. {option2 -> -sub}];
                                        Break[];
                                        ,
                                        chosen2 = 1;
                                        solutionaddedterms[[iaddedterms]] = listProduct[ \
                                          successfullyfoundneg2 /. {option2 -> -sub}];
                                        Break[];
                                    ];
                                ];
                            ];
                            
                            
                            If[chosen2 == 0,
                                solutionaddedterms[[iaddedterms]] = listProduct[ \
                                  successfullyfound[[ieqsection, iaddedterms, \
                                  1]]] /. {option2 -> lookfor};
                            ];
                        ];
                        
                        solution[[i]] = Expand[Join[solution[[i]], \
                          {Total[solutionaddedterms]}]];
                    ];
                    
                ];
                
                
                
                looking = 0;
                For[ichecksoleq = 1, ichecksoleq <= Length[solution], ichecksoleq++,
                    For[ichecksolsec = 1, ichecksolsec <= Length[solution[[ichecksoleq]]], \
                      ichecksolsec++,
                        
                        If[GlobalAnticipateGroupingSmallVariableTermsForDoubleDot == "On",
                            solutiondummy = getterminfo[solution[[ichecksoleq, ichecksolsec]], \
                              fastIV, scalars, newvectors, tensors, closurevar, graddenoters, \
                              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                              divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                              levelcountdummy, {}];
                            solution[[ichecksoleq, ichecksolsec]] = MakeSingleTerm[solutiondummy, \
                              {}, dotdenoters, tensorproductdenoters, \
                              doubledotdenoters][[1, 1, 1]];
                            ,
                            solutiondummy = getterminfo[solution[[ichecksoleq, ichecksolsec]], \
                              {}, scalars, newvectors, tensors, closurevar, graddenoters, \
                              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                              divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                              levelcountdummy, {}];
                            solution[[ichecksoleq, ichecksolsec]] = MakeSingleTerm[solutiondummy, \
                              {}, dotdenoters, tensorproductdenoters, \
                              doubledotdenoters][[1, 1, 1]];
                            ,
                            solutiondummy = getterminfo[solution[[ichecksoleq, ichecksolsec]], \
                              {}, scalars, newvectors, tensors, closurevar, graddenoters, \
                              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                              divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                              levelcountdummy, {}];
                            solution[[ichecksoleq, ichecksolsec]] = MakeSingleTerm[solutiondummy, \
                              {}, dotdenoters, tensorproductdenoters, \
                              doubledotdenoters][[1, 1, 1]];
                        ];
                    ];
                ];
                
                ,
                
                solution[[i]] = main[term[[i]], subin, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, 0, option2, option3];
                
            ];
        ];
        
        Return[solution];
    ];


caseNonListmain[term_, lookfor_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, option_, option2_, option3_] :=
    Block[{i, ii, solution, solutionvec, dummy},
        
        If[Head[term] == Plus,
            solutionvec = ConstantArray[0, Length[term]];
            solution = {};
            For[i = 1, i <= Length[term], i++,
                dummy = casePlusSubstituter[term[[i]], lookfor, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, option, option2];
                solution = Join[solution, dummy];
            ];
            ,
            solution = casePlusSubstituter[term, lookfor, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              option, option2];
            ,
            solution = casePlusSubstituter[term, lookfor, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              option, option2];
        ];
        
        Return[solution];
    ];


casePlusSubstituter[term_, lookfor_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, \
gradform_, normalvecform_, Uform_, slowIV_, fastIV_, option_, option2_] :=
    Block[{solution, levelcountdummy = 0, i, lookforglobal, lookforinfoglobal, option2global, \
    looking, lookforinfoglobalexpanded},
        
        lookforglobal = lookfor[[1, 1, 1, 1]];
        lookforinfoglobal = lookfor[[1]];
        option2global = option2;
        lookforinfoglobalexpanded = lookfor[[2]];
        looking = lookfor[[3]];
        
        solution = getterminfo[term, {}, scalars, vectors, tensors, closurevar, \
          graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
          gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, {}];
        
        Return[solution];
    ];


checkcaseTimes[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef, i, iprevious, totallength, passedterm, inow, testterm, totalterm, \
    multiplier, iremove, icheck},
        
        iprevious = option[[1]];
        
        If[iprevious == 0,
            passedterm = {{{0}, {0}, {0}}};
            totalterm = term;
            ,
            passedterm = term;
            totalterm = option[[2]];
        ];
        
        inow = iprevious + 1;
        
        If[inow > Length[totalterm[[1, 1]]],
            testterm = MakeSingleTerm[passedterm, {}, dotdenoters, tensorproductdenoters, \
              doubledotdenoters];
            If[testterm[[1, 1, 1]] == lookforglobal,
                dimlesscoef = passedterm;
                Return[dimlesscoef];
                ,
                dimlesscoef = {{{-1}}};
                Return[dimlesscoef];
                ,
                dimlesscoef = {{{-1}}};
                Return[dimlesscoef];
            ];
        ];
        
        For[i = inow, i <= Length[totalterm[[1, 1]]], i++,
            If[inow == 1,
                passedterm[[1, 1, 1]] = totalterm[[1, 1, i]];
                passedterm[[1, 2, 1]] = totalterm[[1, 2, i]];
                passedterm[[1, 3, 1]] = totalterm[[1, 3, i]];
                ,
                passedterm[[1, 1]] = Join[passedterm[[1, 1]], {totalterm[[1, 1, i]]}];
                passedterm[[1, 2]] = Join[passedterm[[1, 2]], {totalterm[[1, 2, i]]}];
                passedterm[[1, 3]] = Join[passedterm[[1, 3]], {totalterm[[1, 3, i]]}];
            ];
            
            dimlesscoef = checkcaseTimes[passedterm, isdependenton, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, {i, totalterm}];
            
            If[inow != 1,
                passedterm[[1, 1]] = Drop[passedterm[[1, 1]], -1];
                passedterm[[1, 2]] = Drop[passedterm[[1, 2]], -1];
                passedterm[[1, 3]] = Drop[passedterm[[1, 3]], -1];
            ];
            
            If[Length[dimlesscoef[[1]]] == 1,
                ,
                Break[];
            ];
        ];
        
        
        If[inow == 1 && Length[dimlesscoef[[1]]] != 1,
            multiplier = term;
            For[iremove = 1, iremove <= Length[dimlesscoef[[1, 1]]], iremove++,
                For[icheck = 1, icheck <= Length[multiplier[[1, 1]]], icheck++,
                    If[dimlesscoef[[1, 1, iremove]] == multiplier[[1, 1, icheck]],
                        multiplier[[1, 1]] = Drop[multiplier[[1, 1]], {icheck}];
                        multiplier[[1, 2]] = Drop[multiplier[[1, 2]], {icheck}];
                        multiplier[[1, 3]] = Drop[multiplier[[1, 3]], {icheck}];
                        Break[];
                    ];
                ];
            ];
            dimlesscoef = lookforinfoglobal;
            If[looking == 1,
                dimlesscoef[[1, 1, 1]] = option2global;
            ];
            dimlesscoef[[1, 1]] = Join[dimlesscoef[[1, 1]], multiplier[[1, 1]]];
            dimlesscoef[[1, 2]] = Join[dimlesscoef[[1, 2]], multiplier[[1, 2]]];
            dimlesscoef[[1, 3]] = Join[dimlesscoef[[1, 3]], multiplier[[1, 3]]];
        ];
        If[inow == 1 && Length[dimlesscoef[[1]]] == 1,
            dimlesscoef = term;
        ];
        
        Return[dimlesscoef];
    ];


checkListInList[term_, largerlist_] :=
    Block[{dimlesscoef, i, ii, termdummy, termfound, deleteindices},
        
        If[Length[term] > Length[largerlist],
            Return[False];
        ];
        If[Complement[DeleteDuplicates[term], \
          Intersection[DeleteDuplicates[term], DeleteDuplicates[largerlist]]] != {},
            Return[False];
        ];
        
        termfound = {};
        deleteindices = {};
        For[i = 1, i <= Length[term], i++,
            For[ii = 1, ii <= Length[largerlist], ii++,
                If[term[[i]] == largerlist[[ii]] && Intersection[deleteindices, {{ii}}] == {},
                    deleteindices = Join[deleteindices, {{ii}}];
                    termfound = Join[termfound, {largerlist[[ii]]}];
                    Break[];
                ];
            ];
        ];
        termdummy = largerlist;
        termdummy = Delete[termdummy, deleteindices];
        
        If[term == termfound,
            dimlesscoef = True;
            ,
            dimlesscoef = False;
            ,
            dimlesscoef = False;
        ];
        
        Return[dimlesscoef];
    ];


checkcaseTimes1[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef, i, ii, termdummy, termfound, deleteindices},
        
        If[Length[lookforinfoglobalexpanded[[1, 1]]] > Length[term[[1, 1]]],
            Return[term];
        ];
        If[Complement[DeleteDuplicates[lookforinfoglobalexpanded[[1, 1]]], \
          Intersection[DeleteDuplicates[lookforinfoglobalexpanded[[1, 1]]], DeleteDuplicates[term[[1, 1]]]]] != {},
            Return[term];
        ];
        
        (*Test 2*)
        termdummy = term;
        termfound = {{{}, {}, {}}};
        deleteindices = {};
        For[i = 1, i <= Length[lookforinfoglobalexpanded[[1, 1]]], i++,
            For[ii = 1, ii <= Length[termdummy[[1, 1]]], ii++,
                If[lookforinfoglobalexpanded[[1, 1, i]] == termdummy[[1, 1, ii]] && \
                  Intersection[deleteindices, {{ii}}] == {},
                    deleteindices = Join[deleteindices, {{ii}}];
                    termfound[[1, 1]] = Join[termfound[[1, 1]], {termdummy[[1, 1, ii]]}];
                    termfound[[1, 2]] = Join[termfound[[1, 2]], {termdummy[[1, 2, ii]]}];
                    termfound[[1, 3]] = Join[termfound[[1, 3]], {termdummy[[1, 3, ii]]}];
                    Break[];
                ];
            ];
        ];
        termdummy[[1, 1]] = Delete[termdummy[[1, 1]], deleteindices];
        termdummy[[1, 2]] = Delete[termdummy[[1, 2]], deleteindices];
        termdummy[[1, 3]] = Delete[termdummy[[1, 3]], deleteindices];
        
        If[lookforinfoglobalexpanded == termfound,
            dimlesscoef = lookforinfoglobal;
            dimlesscoef[[1, 1, 1]] = option2global;
            dimlesscoef[[1, 1]] = Join[dimlesscoef[[1, 1]], termdummy[[1, 1]]];
            dimlesscoef[[1, 2]] = Join[dimlesscoef[[1, 2]], termdummy[[1, 2]]];
            dimlesscoef[[1, 3]] = Join[dimlesscoef[[1, 3]], termdummy[[1, 3]]];
            ,
            dimlesscoef = term;
            ,
            dimlesscoef = term;
        ];
        
        Return[dimlesscoef];
    ];


checkcaseTimes2[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef, i, iprevious, totallength, passedterm, inow, testterm, totalterm, \
    multiplier, iremove, icheck, permLimit},
        
        iprevious = option[[1]];
        inow = iprevious + 1;
        permLimit = option[[2]];
        totalterm = option[[3]];
        If[iprevious == 0,
            passedterm = {{{}, {}, {}}};
            ,
            passedterm = term;
        ];
        
        If[inow > permLimit,
            testterm = MakeSingleTerm[passedterm, {}, dotdenoters, tensorproductdenoters, \
              doubledotdenoters];
            If[testterm[[1, 1, 1]] == lookforglobal,
                dimlesscoef = passedterm;
                Return[dimlesscoef];
                ,
                dimlesscoef = {{{-1}}};
                Return[dimlesscoef];
                ,
                dimlesscoef = {{{-1}}};
                Return[dimlesscoef];
            ];
        ];
        
        For[i = inow, i <= Length[totalterm[[1, 1]]], i++,
            passedterm[[1, 1]] = Join[passedterm[[1, 1]], {totalterm[[1, 1, i]]}];
            passedterm[[1, 2]] = Join[passedterm[[1, 2]], {totalterm[[1, 2, i]]}];
            passedterm[[1, 3]] = Join[passedterm[[1, 3]], {totalterm[[1, 3, i]]}];
            
            dimlesscoef = checkcaseTimes2[passedterm, isdependenton, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, {inow, permLimit, totalterm}];
            
            passedterm[[1, 1]] = Drop[passedterm[[1, 1]], -1];
            passedterm[[1, 2]] = Drop[passedterm[[1, 2]], -1];
            passedterm[[1, 3]] = Drop[passedterm[[1, 3]], -1];
            
            If[Length[dimlesscoef[[1]]] == 1,
                ,
                Break[];
            ];
        ];
        
        
        If[inow == 1 && Length[dimlesscoef[[1]]] != 1,
            multiplier = term;
            For[iremove = 1, iremove <= Length[dimlesscoef[[1, 1]]], iremove++,
                For[icheck = 1, icheck <= Length[multiplier[[1, 1]]], icheck++,
                    If[dimlesscoef[[1, 1, iremove]] == multiplier[[1, 1, icheck]],
                        multiplier[[1, 1]] = Drop[multiplier[[1, 1]], {icheck}];
                        multiplier[[1, 2]] = Drop[multiplier[[1, 2]], {icheck}];
                        multiplier[[1, 3]] = Drop[multiplier[[1, 3]], {icheck}];
                        Break[];
                    ];
                ];
            ];
            dimlesscoef = lookforinfoglobal;
            If[looking == 1,
                dimlesscoef[[1, 1, 1]] = option2global;
            ];
            dimlesscoef[[1, 1]] = Join[dimlesscoef[[1, 1]], multiplier[[1, 1]]];
            dimlesscoef[[1, 2]] = Join[dimlesscoef[[1, 2]], multiplier[[1, 2]]];
            dimlesscoef[[1, 3]] = Join[dimlesscoef[[1, 3]], multiplier[[1, 3]]];
        ];
        If[inow == 1 && Length[dimlesscoef[[1]]] == 1,
            dimlesscoef = term;
        ];
        
        Return[dimlesscoef];
    ];


getterminfo[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, levelcount = levelcountdummy + 1, chosen = 0, \
    dependencies, i, flagsubscript = 0},
        
        
        
        chosen2 = 0;
        If[term == lookforglobal,
            dimlesscoef = lookforinfoglobal;
            If[looking == 1,
                dimlesscoef[[1, 1, 1]] = option2global;
            ];
            
            Return[dimlesscoef];
        ];
        
        
        printouttoggle = 0;
        
        If[printouttoggle == 1,
            Print[term];                
        ];
        If[levelcount == 50,
            
            Print["getterminfo: Reached levelcount limit. Backing out."];
            
            ,
            
            If[Head[term] == List && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified List"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseList[term, isdependenton, scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Rational && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified Rational"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseRational[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Integer && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified Integer"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseInteger[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Symbol && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified Symbol"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseSymbol[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[pietrzykflipdenoters[[1]]] && chosen == 0,
                If[term[[2]] == pietrzykflipdenoters[[2]],
                    If[printouttoggle == 1,
                        Print["getterminfo: Identified Pietrzyk Flip"];                
                    ];
                    chosen = 1;
                    dimlesscoef = subsubcasePietrzykFlip[term, isdependenton, scalars, \
                      vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, \
                      derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                ];
            ];
            If[Head[term] == Power && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified Power"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcasePower[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[derivativedenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified Derivative"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseDerivative[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Times && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified Times"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseTimes[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[dotdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified Dot"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseDot[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[tensorproductdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified TensorProduct"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseTensorProduct[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[doubledotdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified DoubleDot"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcaseDoubleDot[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Plus && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified Plus"];                
                ];
                chosen = 1;
                dimlesscoef = subsubcasePlus[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[graddenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified Gradient"];
                ];
                chosen = 1;
                dimlesscoef = subsubcaseGradient[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[divdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified Divergence"];
                ];
                chosen = 1;
                dimlesscoef = subsubcaseDivergence[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[avgdenoters[[1, 1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified Average"];
                ];
                chosen = 1;
                dimlesscoef = subsubcaseAverage[term, isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            
            If[chosen == 0,
                If[printouttoggle == 1,
                    Print["getterminfo: Identified None"];                
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
                  Uform, slowIV, fastIV, levelcountdummy, dependencies];
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
subsubcaseList[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{},
        Print["CRITICAL ERROR: subsubcaseList: Not written yet (but there shouldnt be any lists). Term is: ", term];
        Return[Null];
    ];


(*Ready*)
subsubcaseRational[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{term}, {0}, {0}}}},
        Return[dimlesscoef];
    ];


(*Ready*)
subsubcaseInteger[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{term}, {0}, {0}}}},
        Return[dimlesscoef];
    ];


(*Ready*)
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
               Print["subsubcaseSymbol: variable unidentified as a scalar or vector. Assuming it is a scalar. Variable is: " term];
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
               Print["subsubcaseSymbol: variable unidentified as a scalar, vector, or tensor. Assuming it is a scalar. Variable is: " term2];
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
                    Print["subsubcaseSymbol: a variables was found to not dependent on all independent variables in 'isdependenton'. Assuming it is a function of the independent variables, in general."];
                    dimlesscoef[[1, 3]] = {1};
                ];
            ];
            
            
            ,
            
            
            Print["CRITICAL ERROR: subsubcaseSymbol: variable 'option' was not identified. Term was: ", term2];
            Return[Null];
            
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
subsubcasePietrzykFlip[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, argument = term[[1]], superscript = term[[2]], inthepower, i, j, \
    expandedterm, expandedtermparts, newinthepower = {{{}, {}, {}}}, chosen2 = 0, \
    dimlesscoefparts = {}, bracketeddummy, iii, dummy, scalarmults = {{{}, {}, {}}}},
        
        expandedterm = Expand[argument];
        
        If[Head[expandedterm] == Plus,
            dimlesscoefparts = {};
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
        
        dimlesscoef = {};
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
        If[dimlesscoef == {},
            inthepower = MakeSingleTerm[inthepower, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            dimlesscoef = {{{0}, {inthepower[[1, 2, 1]]}, {0}}};
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
subsubcasePower[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, argument = term[[1]], power = term[[2]], inthepower, i, j, \
    expandedterm = Expand[term], expandedtermparts, newinthepower = 0, chosen2 = 0, ii},
        
        If[Head[expandedterm] == Plus,
            
            dimlesscoef = getterminfo[expandedterm, isdependenton, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
              gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            
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
            
            
            
            If[looking == 1,
                
                If[Abs[power] > 1 && Head[power] == Integer,
                    For[i = 1, i <= Length[dimlesscoef[[1, 1]]], i++,
                        dimlesscoef[[1, 1, i]] = dimlesscoef[[1, 1, i]]^(power/Abs[power]);
                    ];
                    For[i = 2, i <= Abs[power], i++,
                        For[ii = 1, ii <= Length[inthepower[[1, 1]]], ii++,
                            dimlesscoef[[1, 1]] = Join[dimlesscoef[[1, 1]], {dimlesscoef[[1, 1, ii]]}];
                            dimlesscoef[[1, 2]] = Join[dimlesscoef[[1, 2]], {dimlesscoef[[1, 2, ii]]}];
                            dimlesscoef[[1, 3]] = Join[dimlesscoef[[1, 3]], {dimlesscoef[[1, 3, ii]]}];
                        ];
                    ];
                    ,
                    If[Head[power] == Real || Head[power] == Rational || Head[power] == Integer,
                        For[i = 1, i <= Length[dimlesscoef[[1, 1]]], i++,
                            dimlesscoef[[1, 1, i]] = dimlesscoef[[1, 1, i]]^power;
                        ];
                        ,
                        Print["CRITICAL ERROR: subsubcasePower: the power is not a number. Term is: ", term];
                        Return[Null];
                        ,
                        Print["CRITICAL ERROR: subsubcasePower: the power is not a number. Term is: ", term];
                        Return[Null];
                    ];
                    ,
                    If[Head[power] == Real || Head[power] == Rational || Head[power] == Integer,
                        For[i = 1, i <= Length[dimlesscoef[[1, 1]]], i++,
                            dimlesscoef[[1, 1, i]] = dimlesscoef[[1, 1, i]]^power;
                        ];
                        ,
                        Print["CRITICAL ERROR: subsubcasePower: the power is not a number. Term is: ", term];
                        Return[Null];
                        ,
                        Print["CRITICAL ERROR: subsubcasePower: the power is not a number. Term is: ", term];
                        Return[Null];
                    ];
                ];
                
                ,
                
                For[i = 1, i <= Length[dimlesscoef[[1, 1]]], i++,
                    dimlesscoef[[1, 1, i]] = dimlesscoef[[1, 1, i]]^power;
                ];
                
                ,
                
                For[i = 1, i <= Length[dimlesscoef[[1, 1]]], i++,
                    dimlesscoef[[1, 1, i]] = dimlesscoef[[1, 1, i]]^power;
                ];
                
            ];
            
            
        ];
        If[chosen2 == 0,
            Print["CRITICAL ERROR: subsubcasePower: Unable to identify the number of terms coming back. Term is: ", term];
            Return[Null];
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
subsubcaseDerivative[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, subscript = term[[2]], argument = term[[1]], \
    flag = 0, argumentparts, i, expandedargument, dimlesscoefparts, iii, bracketeddummy, \
    argumentpartsbracketed = {}},
        
        expandedargument = Expand[argument];
        
        If[Head[expandedargument] == Plus,
            dimlesscoefparts = {};
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
        
        
        
        
        
        argumentparts = getterminfo[argument, {subscript}, scalars, vectors, tensors, \
          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
          slowIV, fastIV, levelcountdummy, option];
        
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
        If[dimlesscoef == {},
            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            dimlesscoef = {{{0}, {argumentparts[[1, 2, 1]]}, {0}}};
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
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


(*Ready*)
subsubcaseDerivativeSimpSimp[term_, subscript_, isdependenton_, scalars_, vectors_, \
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
                    
                    termdotunfold2 = termdotunfold;
                    
                    For[i = 1, i <= Length[termdotunfold[[1, 1, 1]]], i++,
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
            
            (*If the argument of the derivative is a double dot product, the derivative can be 
            distributed.*)
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == Head[doubledotdenoters[[1]]],
                    chosen = 1;
                    
                    termdotunfold = UnfoldMergedTerms[{term[[iterms]]}, doubledotdenoters, \
                      {subscript}, scalars, vectors, tensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
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
                        
                        (*Adjust scalar, vector, or tensor indication*)
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
                
                (*Adjust scalar, vector, or tensor indication*)
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


(*Ready*)
subsubcaseTimes[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{}, {}, {}}}, expandedterm = Expand[term], i, j, dummy, \
    camebackSum = {}, newterm, flagsubin = 0, subinindex = 0, dimlesscoef2 = {{{}, {}, {}}}, \
    flaginteger = 0, integerindex = 0, chosen = 0, integer},
    
        If[Head[expandedterm] == Plus,
            
            dimlesscoef = getterminfo[expandedterm, isdependenton, scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, levelcountdummy, option];
            
            Return[dimlesscoef];
        ];
        
        
        
        
        
        dimlesscoef = {{{}, {}, {}}};
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
            
            
            
            ,
            
            If[looking == 1,
                
                (*If there is a negative number, sub out the -1 from it.*)
                For[i = 1, i <= Length[dimlesscoef[[1, 1]]], i++,
                    If[Head[dimlesscoef[[1, 1, i]]] == Integer,
                        If[dimlesscoef[[1, 1, i]] != -1 && dimlesscoef[[1, 1, i]] < 0,
                            dimlesscoef[[1, 1, i]] = dimlesscoef[[1, 1, i]] * (-1);
                            dimlesscoef[[1, 1]] = Insert[dimlesscoef[[1, 1]], -1, i + 1];
                            dimlesscoef[[1, 2]] = Insert[dimlesscoef[[1, 2]], 0, i + 1];
                            dimlesscoef[[1, 3]] = Insert[dimlesscoef[[1, 3]], 0, i + 1];
                        ];
                    ];
                ];
                
                If[lookingfirsttime == 0,
                    dimlesscoef = checkcaseTimes1[dimlesscoef, isdependenton, scalars, vectors, \
                      tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                      dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                      normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                      {0, Length[dimlesscoef[[1, 1]]]}];
                ];
                
                flagsubin = 0;
                flaginteger = 0;
                subinindex = 0;
                integerindex = 0;
                For[i = 1, i <= Length[dimlesscoef[[1, 1]]], i++,
                    If[dimlesscoef[[1, 1, i]] == option2global,
                        subinindex = i;
                    ];
                    If[Head[dimlesscoef[[1, 1, i]]] == Integer && dimlesscoef[[1, 1, i]] > 1,
                        flaginteger = 1;
                        integerindex = i;
                    ];
                    If[Head[dimlesscoef[[1, 1, i]]] == Integer && dimlesscoef[[1, 1, i]] < -1,
                        Print["Potential Issue: subsubcaseTimes: Large negative number should have had -1 multiplied out for full analyze of the term. Term is: ", term];
                    ];
                ];
                If[flaginteger == 1,
                    dimlesscoef2 = {{{}, {}, {}}};
                    For[i = 1, i <= Length[dimlesscoef[[1, 1]]], i++,
                        chosen = 0;
                        If[i == subinindex,
                            chosen = 1;
                            dimlesscoef2[[1, 1]] = Join[dimlesscoef2[[1, 1]], \
                              {option2global}];
                            dimlesscoef2[[1, 2]] = Join[dimlesscoef2[[1, 2]], \
                              lookforinfoglobal[[1, 2]]];
                            dimlesscoef2[[1, 3]] = Join[dimlesscoef2[[1, 3]], \
                              lookforinfoglobal[[1, 3]]];
                        ];
                        If[i == integerindex,
                            chosen = 1;
                            integer = dimlesscoef[[1, 1, i]];
                        ];
                        If[chosen == 0,
                            dimlesscoef2[[1, 1]] = Join[dimlesscoef2[[1, 1]], \
                              {dimlesscoef[[1, 1, i]]}];
                            dimlesscoef2[[1, 2]] = Join[dimlesscoef2[[1, 2]], \
                              {dimlesscoef[[1, 2, i]]}];
                            dimlesscoef2[[1, 3]] = Join[dimlesscoef2[[1, 3]], \
                              {dimlesscoef[[1, 3, i]]}];
                        ];
                    ];
                    
                    dimlesscoef[[1, 1]] = Delete[dimlesscoef[[1, 1]], integerindex];
                    dimlesscoef[[1, 2]] = Delete[dimlesscoef[[1, 2]], integerindex];
                    dimlesscoef[[1, 3]] = Delete[dimlesscoef[[1, 3]], integerindex];
                    
                    For[i = 1, i <= integer - 1, i++,
                        dimlesscoef = Join[dimlesscoef, dimlesscoef2];
                    ];
                ];
                
            ];
            
            
            
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
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
        
        
        
        
        
        dimlesscoef = {{{{}}, {{}}, {{}}}};
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
                If[dimlesscoef[[1, 1, 1, i]] == 0,
                    chosen3 = 1;
                    dimlesscoef[[1, 1, 1]] = 0;
                    For[irank = 1, irank <= Length[dimlesscoef[[1, 2, 1]]], irank++,
                        If[irank == 1,
                            dummy = dimlesscoef[[1, 2, 1, irank]];
                            ,
                            dummy = Abs[dummy - dimlesscoef[[1, 2, 1, irank]]];
                        ];
                    ];
                    dimlesscoef[[1, 2, 1]] = dummy;
                    dimlesscoef[[1, 3, 1]] = 0;
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
            
            If[Length[dropindices] > 0,
                dimlesscoef[[1, 1, 1]] = Delete[dimlesscoef[[1, 1, 1]], dropindices];
                dimlesscoef[[1, 2, 1]] = Delete[dimlesscoef[[1, 2, 1]], dropindices];
                dimlesscoef[[1, 3, 1]] = Delete[dimlesscoef[[1, 3, 1]], dropindices];
            ];
            
            dimlesscoef = subsubcaseDotSimp[dimlesscoef, isdependenton, scalars, vectors, \
              tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, levelcountdummy, option];
            
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
subsubcaseDotSimp[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
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


(*Ready*)
subsubcaseTensorProduct[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{{}}, {{}}, {{}}}}, i, ii, j, dummy, iii, \
    camebackSum = 0, distributedtensor, expandedterm, tensorexpandedterm, chosen3 = 0},
        
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
        
        
        
        
        
        dimlesscoef = {{{{}}, {{}}, {{}}}};
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
            For[i = 1, i <= Length[dimlesscoef[[1, 2, 1]]], i++,
                chosen3 = 0;
                If[dimlesscoef[[1, 1, 1, i]] == 0,
                    chosen3 = 1;
                    dimlesscoef[[1, 1, 1]] = 0;
                    dimlesscoef[[1, 2, 1]] = Total[dimlesscoef[[1, 2, 1]]];
                    dimlesscoef[[1, 3, 1]] = 0;
                    Break[];
                ];
                If[dimlesscoef[[1, 2, 1, i]] == 0 && chosen3 == 0,
                    Print["CRITICAL ERROR: subsubcaseTensorProduct: Scalars found in the tensor product. Term is: ", term];
                    Return[Null];
                ];
            ];
            
            dimlesscoef = MergeUnfoldedTerms[dimlesscoef, tensorproductdenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            
        ];
        
        Return[dimlesscoef];
    ];


subsubcaseDoubleDot[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{{}}, {{}}, {{}}}}, i, ii, j, dummy, iii, irank, dropindices, \
    camebackSum = 0, distributeddoubledot, expandedterm, doubledotexpandedterm, chosen3 = 0},
        
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
        
        
        
        
        
        dimlesscoef = {{{{}}, {{}}, {{}}}};
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
                If[dimlesscoef[[1, 1, 1, i]] == 0,
                    chosen3 = 1;
                    dimlesscoef[[1, 1, 1]] = 0;
                    For[irank = 1, irank <= Length[dimlesscoef[[1, 2, 1]]], irank++,
                        If[irank == 1,
                            dummy = dimlesscoef[[1, 2, 1, irank]];
                            ,
                            dummy = Abs[dummy - dimlesscoef[[1, 2, 1, irank]]];
                        ];
                    ];
                    dimlesscoef[[1, 2, 1]] = dummy;
                    dimlesscoef[[1, 3, 1]] = 0;
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
            
            If[Length[dropindices] > 0,
                dimlesscoef[[1, 1, 1]] = Delete[dimlesscoef[[1, 1, 1]], dropindices];
                dimlesscoef[[1, 2, 1]] = Delete[dimlesscoef[[1, 2, 1]], dropindices];
                dimlesscoef[[1, 3, 1]] = Delete[dimlesscoef[[1, 3, 1]], dropindices];
            ];
            
            dimlesscoef = MergeUnfoldedTerms[dimlesscoef, doubledotdenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
subsubcasePlus[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, i, termparts, expandedterm = Expand[term]},
        
        If[Head[expandedterm] == Plus,
            For[i = 1, i <= Length[expandedterm], i++,
                termparts = getterminfo[expandedterm[[i]], isdependenton, scalars, vectors, \
                  tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
                
                dimlesscoef = Join[dimlesscoef, termparts];
            ];
            ,
            Print["CRITICAL ERROR: subsubcasePlus: Expanded sum became a non-Plus. Term is: ", term];
            Return[Null];
            ,
            Print["CRITICAL ERROR: subsubcasePlus: Expanded sum became a non-Plus. Term is: ", term];
            Return[Null];
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
subsubcaseGradient[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, subscript = term[[2]], argument = term[[1]], \
    flag = 0, argumentparts, i, expandedargument, dimlesscoefparts, iii, bracketeddummy, \
    argumentpartsbracketed = {}},
        
        expandedargument = Expand[argument];
        
        If[Head[expandedargument] == Plus,
            dimlesscoefparts = {};
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
        
        
        
        
        
        argumentparts = getterminfo[argument, subscript, scalars, vectors, tensors, \
          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
          slowIV, fastIV, levelcountdummy, option];
        
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
        If[dimlesscoef == {},
            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            dimlesscoef = {{{0}, {argumentparts[[1, 2, 1]] + 1}, {0}}};
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
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


(*Ready*)
subsubcaseGradientSimpSimp[term_, subscript_, isdependenton_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{i, chosen = 0, iterms, newargumentparts2 = {}, flag1 = 0, flag2 = 0, scalar, \
    vector, termdotunfold, chosen2 = 0, chosen3 = 0, term2, termdotunfold2},
        
        For[iterms = 1, iterms <= Length[term], iterms++,
            chosen = 0;
            
            (*If the argument of the gradient is a dot product, where the gradient being
            applied can move onto one component in the dot product.*)
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == Head[dotdenoters[[1]]],
                    
                    termdotunfold = UnfoldMergedTerms[{term[[iterms]]}, dotdenoters, \
                      subscript, scalars, vectors, tensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
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
            
            
            (*If the argument of the gradient is a double dot product, where the gradient being
            applied can move onto one component in the double dot product.*)
            If[Length[term[[iterms, 1]]] == 1 && chosen == 0,
                If[Head[term[[iterms, 1, 1]]] == Head[doubledotdenoters[[1]]],
                    
                    termdotunfold = UnfoldMergedTerms[{term[[iterms]]}, doubledotdenoters, \
                      subscript, scalars, vectors, tensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                      doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
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
            
            
            If[chosen == 0,
                newargumentparts2 = Join[newargumentparts2, {term[[iterms]]}];
                
                (*Log the term*)
                newargumentparts2[[-1, 1]] = {OperatorApplier[listProduct[ \
                  newargumentparts2[[-1, 1]]], graddenoters, subscript]};
                
                (*Adjust scalar, vector, or tensor indication*)
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


(*Ready*)
subsubcaseDivergence[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, subscript = term[[2]], argument = term[[1]], \
    flag = 0, argumentparts, i, expandedargument, dimlesscoefparts, iii, bracketeddummy, \
    argumentpartsbracketed},
        
        expandedargument = Expand[argument];
        
        If[Head[expandedargument] == Plus,
            dimlesscoefparts = {};
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
        
        
        
        
        
        argumentparts = getterminfo[argument, subscript, scalars, vectors, tensors, \
          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
          gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
        
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
        If[dimlesscoef == {},
            If[argumentparts == {{{0}, {0}, {0}}},
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


(*Ready*)
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
                Print["CRITICAL ERROR: subsubcaseDivergenceSimp: Divergence is being taken on a scalar. The term is: ", argumentparts];
                Return[Null];
            ];
            ,
            If[scalarmults == {{{0}, {0}, {0}}},
                Return[scalarmults];
            ];
            Print["CRITICAL ERROR: subsubcaseDivergenceSimp: Divergence was taken on scalar. The term is: ", argumentparts];
            Return[Null];
        ];
        
        (*Check to see if there is anything with the correct dependency in the divergence.*)
        If[Total[newargumentparts[[1, 3]]] == 0,
            dimlesscoef = {{{0}, {Total[argumentparts[[1, 2]]] - 1}, {0}}};
            Return[dimlesscoef];
        ];
        
        
        (*Decide the new dependency*)
        chosen = 0;
        If[Intersection[subscript, isdependenton] != {},
            chosen = 1;
            
            If[Length[newargumentparts[[1, 1]]] > 0,
                newargumentparts = subsubcaseDivergenceSimpSimp[newargumentparts, \
                  subscript, isdependenton, scalars, vectors, tensors, closurevar, \
                  graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                  doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                  levelcountdummy, option];
                ,
                Print["CRITICAL ERROR: subsubcaseDivergenceSimp: Divergence was taken on scalar. The term is: ", argumentparts];
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
                Print["CRITICAL ERROR: subsubcaseDivergenceSimp: Divergence was taken on scalar. The term is: ", argumentparts];
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


(*Ready*)
subsubcaseDivergenceSimpSimp[term_, subscript_, isdependenton_, scalars_, vectors_, \
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
                
                (*Adjust scalar, vector, or tensor indication*)
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


(*Ready*)
subsubcaseAverage[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, \
graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, subscript = term[[2]], argument = term[[1, 1]], flag, \
    argumentparts, IVofsubscript, ii, argumentpartsbracketed, i, expandedargument, iii, \
    dimlesscoefparts, bracketeddummy, avgdenoterssymbols = avgdenoters[[2, ;;, 1]], \
    avgdenotersIV = avgdenoters[[2, ;;, 2]], izerocheck, avgdenoterssplitdomainsymbols = \
    avgdenoters[[2, ;;, 3]]},
        
        For[i = 1, i <= Length[avgdenoterssymbols], i++,
            If[Intersection[{subscript}, avgdenoterssymbols[[i]]] != {} || \
              Intersection[{subscript}, Flatten[avgdenoterssplitdomainsymbols[[i]]]] != {},
                
                expandedargument = Expand[argument];
                
                If[Head[expandedargument] == Plus,
                    dimlesscoefparts = {};
                    For[ii = 1, ii <= Length[expandedargument], ii++,
                        bracketeddummy = OperatorApplier[expandedargument[[ii]], \
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
                
                
                
                
                
                argumentparts = getterminfo[argument, avgdenoters[[2, i, 2]], scalars, \
                  vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                For[iii = 1, iii <= Length[argumentparts], iii++,
                    argumentpartsbracketed = subsubcaseAverageSimp[{argumentparts[[iii]]}, \
                      subscript, avgdenoters[[2, i, 2]], isdependenton, scalars, vectors, \
                      tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                      dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                      gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                      option];
                    For[izerocheck = 1, izerocheck <= Length[argumentpartsbracketed], \
                      izerocheck++,
                        If[listProduct[argumentpartsbracketed[[izerocheck, 1]]] == 0,
                            ,
                            dimlesscoef = Join[dimlesscoef, \
                              {argumentpartsbracketed[[izerocheck]]}];
                            ,
                            dimlesscoef = Join[dimlesscoef, \
                              {argumentpartsbracketed[[izerocheck]]}];
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
        
        
        Print["CRITICAL ERROR: subsubcaseAverage: Unidentified average. Term is: ", term];
        Return[Null];
    ];


(*Ready*)
subsubcaseAverageSimp[argumentparts_, subscript_, IVofsubscript_, isdependenton_, scalars_, \
vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = {{{}, {}, {}}}, i, scalarmults = {{{}, {}, {}}}, \
    newargumentparts=  {{{}, {}, {}}}, flag1 = 0, flag2 = 0, chosen = 0, scalarmults2, \
    newargumentparts2, dummyargumentparts, dummynewargumentparts, chosen2 = 0, \
    dummynewargumentparts2, iterms, imult, icombine},
        
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
        chosen = 0;
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


(*Ready*)
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
                        
                        (*The average being applied can move onto a few components in the dot
                        product that are next to each other.*)
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
                        
                        (*The average being applied can move onto a few components in the dot
                        product that are next to each other.*)
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


MergeProducts[argumentparts_, bracketform_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_] :=
    Block[{dimlesscoef = argumentparts, i, j},
        
        dimlesscoef = MergeUnfoldedTerms[dimlesscoef, bracketform, dotdenoters, \
          tensorproductdenoters, doubledotdenoters];
        
        dimlesscoef = CorrectTensorProductTerms[dimlesscoef, tensorproductdenoters];
        
        For[i = 1, i <= Length[dimlesscoef], i++,
            For[j = 1, j <= Length[dimlesscoef[[i, 1]]], j++,
                If[dimlesscoef[[i, 1, j]] == 0,
                    If[dimlesscoef[[i, 2, j]] < 0,
                        dimlesscoef[[i, 2, j]] = 0;
                    ];
                ];
            ];
        ];
        
        Return[dimlesscoef];
    ];


MergeUnfoldedTerms[argumentparts_, bracketform_, dotdenoters_, tensorproductdenoters_, \
doubledotdenoters_] :=
    Block[{i, iii, flag, dimlesscoef = argumentparts, chosen, dummy, iterm, flag1, \
    izerocheck, irank, chosen2 = 0},
        
        For[iii = 1, iii <= Length[dimlesscoef], iii++,
            For[i = 1, i <= Length[dimlesscoef[[iii, 1]]], i++,
                If[Head[dimlesscoef[[iii, 1, i]]] == List,
                    
                    chosen = 0;
                    
                    (*If there is a zero in the dot product.*)
                    For[izerocheck = 1, izerocheck <= Length[dimlesscoef[[iii, 1, i]]], \
                      izerocheck++,
                        If[dimlesscoef[[iii, 1, i, izerocheck]] == 0,
                            chosen = 1;
                            
                            (* Implement tensor multiplication*)
                            dimlesscoef[[iii, 1, i]] = 0;
                            
                            (*Adjust scalar, vector, or tensor indication*)
                            chosen2 = 0;
                            If[Head[bracketform[[1]]] == Head[dotdenoters[[1]]] && \
                              chosen2 == 0,
                                chosen2 = 1;
                                For[irank = 1, irank <= Length[dimlesscoef[[iii, 2, i]]], \
                                  irank++,
                                    If[irank == 1,
                                        dummy = dimlesscoef[[iii, 2, i, irank]];
                                        ,
                                        dummy = Abs[dummy - dimlesscoef[[iii, 2, i, irank]]];
                                    ];
                                ];
                                dimlesscoef[[iii, 2, i]] = dummy;
                            ];
                            If[Head[bracketform[[1]]] == Head[doubledotdenoters[[1]]] && \
                              chosen2 == 0,
                                chosen2 = 1;
                                For[irank = 1, irank <= Length[dimlesscoef[[iii, 2, i]]], \
                                  irank++,
                                    If[irank == 1,
                                        dummy = dimlesscoef[[iii, 2, i, irank]];
                                        ,
                                        dummy = Abs[dummy - dimlesscoef[[iii, 2, i, irank]]];
                                    ];
                                ];
                                dimlesscoef[[iii, 2, i]] = dummy;
                            ];
                            If[Head[bracketform[[1]]] == Head[tensorproductdenoters[[1]]] && \
                              chosen2 == 0,
                                chosen2 = 1;
                                dimlesscoef[[iii, 2, i]] = Total[dimlesscoef[[iii, 2, i]]];
                            ];
                            If[chosen2 == 0,
                                Print["CRITICAL ERROR: MergeUnfoldedTerms: Unidentified product operator. Term is: ", argumentparts];
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
                        
                        (*Adjust scalar, vector, or tensor indication*)
                        chosen2 = 0;
                        If[Head[bracketform[[1]]] == Head[dotdenoters[[1]]] && \
                          chosen2 == 0,
                            chosen2 = 1;
                            For[irank = 1, irank <= Length[dimlesscoef[[iii, 2, i]]], \
                              irank++,
                                If[irank == 1,
                                    dummy = dimlesscoef[[iii, 2, i, irank]];
                                    ,
                                    dummy = Abs[dummy - dimlesscoef[[iii, 2, i, irank]]];
                                ];
                            ];
                            dimlesscoef[[iii, 2, i]] = dummy;
                        ];
                        If[Head[bracketform[[1]]] == Head[doubledotdenoters[[1]]] && \
                          chosen2 == 0,
                            chosen2 = 1;
                            For[irank = 1, irank <= Length[dimlesscoef[[iii, 2, i]]], \
                              irank++,
                                If[irank == 1,
                                    dummy = dimlesscoef[[iii, 2, i, irank]];
                                    ,
                                    dummy = Abs[dummy - dimlesscoef[[iii, 2, i, irank]]];
                                ];
                            ];
                            dimlesscoef[[iii, 2, i]] = dummy;
                        ];
                        If[Head[bracketform[[1]]] == Head[tensorproductdenoters[[1]]] && \
                          chosen2 == 0,
                            chosen2 = 1;
                            dimlesscoef[[iii, 2, i]] = Total[dimlesscoef[[iii, 2, i]]];
                        ];
                        If[chosen2 == 0,
                            Print["CRITICAL ERROR: MergeUnfoldedTerms: Unidentified product operator. Term is: ", argumentparts];
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
            Print["CRITICAL ERROR: MakeSingleTerm: Tensors without the same rank are being summed together. The term is: ", argumentparts];
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

