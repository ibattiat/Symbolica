(* ::Package:: *)

(* Symbolica \[Copyright] 2023 by Kyle Pietrzyk is licensed under Attribution-NonCommercial-ShareAlike 4.0 International *)


(****** SolutionFormFinder.wl -- 
*************)

(* 

SolutionFormFinder[term, ] 

*)


(* Created December 6 2020
Modified: 
                       *)



BeginPackage["Disruptioneering`SolutionFormFinder`"];

SolutionFormFinder::usage = 
"SolutionFormFinder[term, ] ";



Begin["`Disruptioneering`"];

(*avgdenoters should be {{Subscript[\[LeftAngleBracket]spaceholder\[RightAngleBracket], sub], sub, spaceholder},
{{{\[CapitalOmega],\[CapitalGamma]},{\[Xi],\[Eta]},{{\[CapitalOmega]a,\[CapitalOmega]b},{\[CapitalGamma]a,\[CapitalGamma]b}}},{{\[CapitalOmega]a,\[CapitalGamma]a},{\[Xi], \[Eta]},{{},{\[CapitalGamma]ai,\[CapitalGamma]ao}}},
{{\[CapitalOmega]b,\[CapitalGamma]b},{\[Xi], \[Eta]},{{},{\[CapitalGamma]bi,\[CapitalGamma]bo}}}{{\[CapitalDelta]\[Tau]PE1,\[Delta]\[Tau]PE1}, {\[Tau]Pe1},{{},{}}}}}*)
(*graddenoters should be {Grad[spaceholder, sub], sub, spaceholder}*)
(*divdenoters should be {Div[spaceholder, sub], sub, spaceholder}*)

SolutionFormFinder[term_, solformterm_, closureprefix_, closuredependencies_, \
alldependencies_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, \
avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, \
pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
option_] := 
    procedure[term, solformterm, closureprefix, closuredependencies, alldependencies, scalars, \
      vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, \
      derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
      pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, option, \
      {}, {}, {}, {}];
      
SolutionFormFinder[term_, solformterm_, closureprefix_, closuredependencies_, \
alldependencies_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, \
avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, \
pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
option_, option2_, option2p5_, option3_, option4_] := 
    procedure[term, solformterm, closureprefix, closuredependencies, alldependencies, \
      scalars, vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, \
      derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
      pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, option, \
      option2, option2p5, option3, option4];


procedure[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
option_, option2_, option2p5_, option3_, option4_] :=
    Block[{solution, uvecdependency, newvectors, i, chosen = 0},
        
        newvectors = vectors;
        
        chosen = 0;
        If[option == "CombineEQBC" && chosen == 0,
            chosen = 1;
            solution = combineclosureproblemsEQBC[Expand[term], solformterm, closureprefix, \
              closuredependencies, alldependencies, scalars, newvectors, tensors, closurevar, \
              graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
              divform, gradform, normalvecform, Uform, slowIV, fastIV, option];
        ];
        If[option == "ConsiderPreviousClosureProblems" && chosen == 0,
            chosen = 1;
            solution = considerpreviousclosureproblems[Expand[term], solformterm, closureprefix, \
              closuredependencies, alldependencies, scalars, newvectors, tensors, closurevar, \
              graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
              divform, gradform, normalvecform, Uform, slowIV, fastIV, option, option2, \
              option2p5, option3, option4];
        ];
        If[option == "ConsolidateClosureForms" && chosen == 0,
            chosen = 1;
            solution = ConsolidateClosureForms[term, solformterm, \
              closureprefix, closuredependencies, alldependencies, scalars, \
              newvectors, tensors, closurevar, graddenoters, divdenoters, \
              avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
              doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, {}];
        ];
        If[chosen == 0,
            chosen = 1;
            solution = main[Expand[term], solformterm, closureprefix, closuredependencies, \
              alldependencies, scalars, newvectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
              gradform, normalvecform, Uform, slowIV, fastIV, 1];
        ];
        
        solution = Expand[solution];
        
        Return[solution];
    ];


main[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
option_] :=
    Block[{solution},
        If[Head[term] == List,
            solution = caseListmain[term, solformterm, closureprefix, closuredependencies, alldependencies, \
              scalars, vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
              dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
              slowIV, fastIV, option];
            ,
            solution = caseNonListmain[term, solformterm, closureprefix, closuredependencies, \
              alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, option];
            ,
            solution = caseNonListmain[term, solformterm, closureprefix, closuredependencies, \
              alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, option];
        ];
        
        Return[solution];
    ];


caseListmain[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
option_] :=
    Block[{i, solution = ConstantArray[0, Length[term]], toplayer = option, dimlesscoef, \
    solformtermindexglobal, expandedterm, ii, closureindexglobal = 0},
        
        expandedterm = Expand[term];
        
        (*Note: toplayer is the equation layer*)
        
        For[i = 1, i <= Length[expandedterm], i++,
            If[toplayer == 1,
                
                closureindexglobal = 0;
                solformtermindexglobal = i;
                
                solution[[i]] = main[term[[i]], {solformterm, solformtermindexglobal}, \
                  closureprefix, {closuredependencies, closureindexglobal}, alldependencies, scalars, \
                  vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
                  slowIV, fastIV, 0];
                
                ,
                
                If[i == 1,
                    closureindexglobal = closuredependencies[[2]];
                ];
                {closureindexglobal, solution[[i]]} = main[term[[i]], solformterm, closureprefix, \
                  {closuredependencies[[1]], closureindexglobal}, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, option];
                
            ];
        ];
        
        Return[solution];
    ];


considerpreviousclosureproblems[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, \
scalars_, vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, \
dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, \
fastIV_, option_, option2_, option2p5_, option3_, option4_] :=
    Block[{dimlesscoef = {}, isection, iterm, chosen = 0, i, ieq, closurecount, \
    scalarclosurevar, movecount, scalarcombinedclosureEQset, baseequations, vectorclosurevar, \
    vectorcombinedclosureEQset, dummy, tensorcombinedclosureEQset, tensorclosurevar, iorder, \
    iclosureeq, isubterm, flag = 0, iversions, combinedpreviousclosureEQset, dropterms, \
    newterm = term, baseforcingterms, vectorclosureform},
        
        (*Find Base Equations*)
        baseequations = ConstantArray[{}, Length[term]];
        For[ieq = 1, ieq <= Length[term], ieq++,
            baseequations[[ieq]] = ConstantArray[0, Length[term[[ieq]]]];
            For[isection = 1, isection <= Length[term[[ieq]]], isection++,
                For[iterm = 1, iterm <= Length[term[[ieq, isection]]], iterm++,
                    If[term[[ieq, isection, iterm, 2]] == {0, 0, 0},
                        baseequations[[ieq, isection]] += term[[ieq, isection, iterm, 1]];
                    ];
                ];
            ];
        ];
        
        
        
        
        (*Find the previously used closure forcing terms*)
        combinedpreviousclosureEQset = ConstantArray[0, Length[option2]];
        dropterms = ConstantArray[0, Length[option2]];
        baseforcingterms = ConstantArray[{}, Length[baseequations]];
        For[iorder = 1, iorder <= Length[option2], iorder++,
            combinedpreviousclosureEQset[[iorder]] = ConstantArray[{}, Length[option2[[iorder]]]];
            dropterms[[iorder]] = ConstantArray[{}, Length[option2[[iorder]]]];
            For[iclosureeq = 1, iclosureeq <= Length[option2[[iorder]]], iclosureeq++,
                For[isubterm = 1, isubterm <= Length[option2[[iorder, iclosureeq]]], isubterm++,
                    
                    flag = 0;
                    For[ieq = 1, ieq <= Length[term], ieq++,
                        For[isection = 1, isection <= Length[term[[ieq]]], isection++,
                            For[iterm = 1, iterm <= Length[term[[ieq, isection]]], iterm++,
                                For[iversions = 2, iversions <= Length[term[[ieq, isection, iterm]]], iversions++,
                                    If[term[[ieq, isection, iterm, 1]] == \
                                      option2[[iorder, iclosureeq, isubterm]] && \
                                      term[[ieq, isection, iterm, iversions, 3]] == option3[[iorder, iclosureeq]],
                                        flag = 1;
                                        
                                        chosen = 0;
                                        If[combinedpreviousclosureEQset[[iorder, iclosureeq]] != {},
                                            chosen = 1;
                                            combinedpreviousclosureEQset[[iorder, iclosureeq, 2, ieq, isection]] += \
                                              term[[ieq, isection, iterm, 1]];
                                            combinedpreviousclosureEQset[[iorder, iclosureeq, 3, ieq]] = Join[ \
                                              combinedpreviousclosureEQset[[iorder, iclosureeq, 3, ieq]], \
                                              {term[[ieq, isection, iterm, 1]]}];
                                        ];
                                        
                                        If[chosen == 0,
                                            combinedpreviousclosureEQset[[iorder, iclosureeq]] = \
                                              Join[combinedpreviousclosureEQset[[iorder, iclosureeq]], \
                                              {{0, 0, 0}, baseequations, baseforcingterms}];
                                            
                                            combinedpreviousclosureEQset[[iorder, iclosureeq, 3, ieq]] = \
                                              Join[combinedpreviousclosureEQset[[iorder, iclosureeq, 3, ieq]], \
                                              {term[[ieq, isection, iterm, 1]]}];
                                            vectorclosurevar = option4[[iorder, iclosureeq]];
                                            vectorclosureform = option2p5[[iorder, iclosureeq]];
                                            combinedpreviousclosureEQset[[iorder, iclosureeq, 1, 1]] = vectorclosureform;
                                            combinedpreviousclosureEQset[[iorder, iclosureeq, 1, 2]] = vectorclosurevar;
                                            combinedpreviousclosureEQset[[iorder, iclosureeq, 1, 3]] = \
                                              term[[ieq, isection, iterm, iversions, 3]];
                                            combinedpreviousclosureEQset[[iorder, iclosureeq, 2, ieq, isection]] += \
                                              term[[ieq, isection, iterm, 1]];
                                        ];
                                        
                                        dropterms[[iorder, iclosureeq]] = Join[dropterms[[iorder, iclosureeq]], \
                                         {{ieq, isection, iterm}}];
                                        
                                        Break[];
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
                        If[flag == 1,
                            Break[];
                        ];
                    ];
                    
                    If[flag == 0,
                        Print["CRITICAL ERROR: considerpreviousclosureproblems: Couldnt find the following term in the input. This means the code will eventually not be able to regroup the terms into the averaged quantities. The term is: ", option2[[iorder, iclosureeq, isubterm]]];
                        Return[Null];
                    ];
                    
                ];
            ];
        ];
        
        newterm = Delete[newterm, Flatten[dropterms, 2]];
        
        Return[{combinedpreviousclosureEQset, newterm}];
    ];


combineclosureproblemsEQBC[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, \
scalars_, vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, \
dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, \
fastIV_, option_] :=
    Block[{dimlesscoef = {}, isection, iterm, chosen = 0, i, ii, ieq, closurecount, \
    scalarclosurevar, movecount, scalarcombinedclosureEQset, baseequations, vectorclosurevar, \
    vectorcombinedclosureEQset, dummy, tensorcombinedclosureEQset, tensorclosurevar, skip, \
    scalarcombinedclosureEQsetEQSCALARS = {}, scalarcombinedclosureEQsetBCSCALARS = {}, \
    forcingtermratio, EQSCALARSclosureformmultiples, BCSCALARSclosureformmultiples, \
    closureformmultiplesratio, newscalarcombinedclosureEQset, \
    EQSCALARSclosureformmultiplesparts, BCSCALARSclosureformmultiplesparts, \
    closureformmultiplesratioparts, newclosureform, Nbaseequations, scalarequationssaved, \
    baseforcingterms, Nforcingterms, iclosprob, sumforcingtermsi, sumforcingtermsii, \
    ratioforcingterms, closureformcoefi, closureformcoefii, newclosureequations, \
    combinedscalarcombinedclosureEQset, newforcingterms, flag1, ratioforcingtermsinfo},
        
        (*Find Base Equations*)
        baseequations = ConstantArray[{}, Length[term]];
        For[ieq = 1, ieq <= Length[term], ieq++,
            baseequations[[ieq]] = ConstantArray[0, Length[term[[ieq]]]];
            For[isection = 1, isection <= Length[term[[ieq]]], isection++,
                For[iterm = 1, iterm <= Length[term[[ieq, isection]]], iterm++,
                    If[term[[ieq, isection, iterm, 2]] == {0, 0, 0},
                        baseequations[[ieq, isection]] += term[[ieq, isection, iterm, 1]];
                    ];
                ];
            ];
        ];
        Nbaseequations = Length[baseequations];
        
        
        
        closurecount = 0;
        
        
        (*Combine similar scalar closure variables throughout equations/BCs.*)
        movecount = 0;
        scalarcombinedclosureEQset = {};
        baseforcingterms = ConstantArray[{}, Length[baseequations]];
        For[ieq = 1, ieq <= Length[term], ieq++,
            For[isection = 1, isection <= Length[term[[ieq]]], isection++,
                For[iterm = 1, iterm <= Length[term[[ieq, isection]]], iterm++,
                    skip = 0;
                    If[term[[ieq, isection, iterm, 2, 1]] == 0,
                        skip = 1;
                    ];
                    If[term[[ieq, isection, iterm, 2, 3]] == 0 && skip == 0,
                        
                        chosen = 0;
                        If[scalarcombinedclosureEQset != {},
                            For[i = 1, i <= Length[scalarcombinedclosureEQset], i++,
                                dummy = term[[ieq, isection, iterm, 2, 1]] /. \
                                  {term[[ieq, isection, iterm, 2, 2]] -> \
                                  scalarcombinedclosureEQset[[i, 1, 2]]};
                                If[scalarcombinedclosureEQset[[i, 1, 1]] == dummy,
                                    chosen = 1;
                                    scalarcombinedclosureEQset[[i, 2, ieq, isection]] += \
                                      term[[ieq, isection, iterm, 1]];
                                    scalarcombinedclosureEQset[[i, 3, ieq]] = Join[ \
                                      scalarcombinedclosureEQset[[i, 3, ieq]], \
                                      {term[[ieq, isection, iterm, 1]]}];
                                    Break[];
                                ];
                            ];
                        ];
                        
                        If[chosen == 0,
                            scalarcombinedclosureEQset = Join[scalarcombinedclosureEQset, \
                              {{{0, 0, 0}, baseequations, baseforcingterms}}];
                            
                            scalarcombinedclosureEQset[[-1, 3, ieq]] = Join[ \
                              scalarcombinedclosureEQset[[-1, 3, ieq]], \
                              {term[[ieq, isection, iterm, 1]]}];
                            
                            
                            scalarclosurevar = ToExpression[ToString[closureprefix] <> ToString[ \
                              closurecount]];
                            scalarclosurevar = dependencyadder[scalarclosurevar, \
                            closuredependencies[[1]]];
                            closurecount += 1;
                            
                            scalarcombinedclosureEQset[[-1, 1, 1]] = \
                              term[[ieq, isection, iterm, 2, 1]] /. \
                              {term[[ieq, isection, iterm, 2, 2]] -> scalarclosurevar};
                            scalarcombinedclosureEQset[[-1, 1, 2]] = scalarclosurevar;
                            scalarcombinedclosureEQset[[-1, 1, 3]] = \
                              term[[ieq, isection, iterm, 2, 3]];
                            scalarcombinedclosureEQset[[-1, 2, ieq, isection]] += \
                              term[[ieq, isection, iterm, 1]];
                        ];
                        
                    ];
                ];
            ];
        ];
        
        
        
        
        flag1 = 0;
        While[Length[scalarcombinedclosureEQset] > 1 && flag1 == 0,
            flag1 = 1;
            
            Nforcingterms = ConstantArray[{}, Length[scalarcombinedclosureEQset]];
            For[iclosprob = 1, iclosprob <= Length[scalarcombinedclosureEQset], iclosprob++,
                For[ieq = 1, ieq <= Length[baseequations], ieq++,
                    Nforcingterms[[iclosprob]] = Join[Nforcingterms[[iclosprob]], \
                      {Length[scalarcombinedclosureEQset[[iclosprob, 3, ieq]]]}];
                ];
            ];
            
            For[i = 1, i <= Length[Nforcingterms] - 1, i++,
                For[ii = i + 1, ii <= Length[Nforcingterms], ii++,
                    If[Nforcingterms[[i]] == Nforcingterms[[ii]],
                        
                        closureformcoefi = Simplify[scalarcombinedclosureEQset[[i, 1, 1]] /. \
                          {scalarcombinedclosureEQset[[i, 1, 2]] -> 1}];
                        closureformcoefii = Simplify[scalarcombinedclosureEQset[[ii, 1, 1]] /. \
                          {scalarcombinedclosureEQset[[ii, 1, 2]] -> 1}];
                        
                        sumforcingtermsi = ConstantArray[{}, Length[baseequations]];
                        sumforcingtermsii = ConstantArray[{}, Length[baseequations]];
                        ratioforcingterms = {};
                        For[ieq = 1, ieq <= Length[baseequations], ieq++,
                            If[Length[scalarcombinedclosureEQset[[i, 3, ieq]]] != 0 && \
                              Length[scalarcombinedclosureEQset[[ii, 3, ieq]]] != 0,
                                sumforcingtermsi[[ieq]] = Simplify[Total[ \
                                  scalarcombinedclosureEQset[[i, 3, ieq]]] /. \
                                  {closureformcoefi -> 1}];
                                sumforcingtermsii[[ieq]] = Simplify[Total[ \
                                  scalarcombinedclosureEQset[[ii, 3, ieq]]] /. \
                                  {closureformcoefii -> 1}];
                                ratioforcingterms = Join[ratioforcingterms, {Simplify[ \
                                  sumforcingtermsi[[ieq]] / sumforcingtermsii[[ieq]]]}];
                            ];
                        ];
                        
                        If[Equal @@ ratioforcingterms,
                            
                            Print["combineclosureproblemsEQBC: If you read a message from subsubcaseSymbol after this message, it is okay. It is expected and occurs due to the dependency analysis of the following ratio: ", ratioforcingterms[[1]]];
                            ratioforcingtermsinfo = getterminfo[ratioforcingterms[[1]], \
                              alldependencies, scalars, vectors, tensors, closurevar, \
                              graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                              dotdenoters, tensorproductdenoters, doubledotdenoters, \
                              pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
                              slowIV, fastIV, 0, {}];
                            ratioforcingtermsinfo = MakeSingleTerm[ratioforcingtermsinfo, {}, \
                              dotdenoters, tensorproductdenoters, doubledotdenoters];
                            
                            If[ratioforcingtermsinfo[[1, 3, 1]] == 0,
                                newclosureform = Simplify[scalarcombinedclosureEQset[[i, 1, 1]] * \
                                  ratioforcingterms[[1]] + scalarcombinedclosureEQset[[ii, 1, 1]] /. \
                                  {scalarcombinedclosureEQset[[ii, 1, 2]] -> \
                                  scalarcombinedclosureEQset[[i, 1, 2]]}];
                                newclosureequations = scalarcombinedclosureEQset[[i, 2]] + \
                                  scalarcombinedclosureEQset[[ii, 2]] - baseequations;
                                newforcingterms = ConstantArray[{}, Length[baseequations]];
                                For[ieq = 1, ieq <= Length[baseequations], ieq++,
                                    newforcingterms[[ieq]] = Join[newforcingterms[[ieq]], \
                                      scalarcombinedclosureEQset[[i, 3, ieq]]];
                                    newforcingterms[[ieq]] = Join[newforcingterms[[ieq]], \
                                      scalarcombinedclosureEQset[[ii, 3, ieq]]];
                                ];
                                
                                scalarcombinedclosureEQset[[i]] = {{newclosureform, \
                                  scalarcombinedclosureEQset[[i, 1, 2]], 0}, newclosureequations, \
                                  newforcingterms};
                                scalarcombinedclosureEQset = Delete[scalarcombinedclosureEQset, ii];
                                
                                closurecount -= 1;
                                flag1 = 0;
                            ];
                            
                        ];
                        
                    ];
                    If[flag1 == 0,
                        Break[];
                    ];
                ];
                If[flag1 == 0,
                    Break[];
                ];
            ];
            
        ];
        For[i = 1, i <= Length[scalarcombinedclosureEQset], i++,
            
            scalarclosurevar = ToExpression[ToString[closureprefix] <> ToString[i - 1]];
            scalarclosurevar = dependencyadder[scalarclosurevar, closuredependencies[[1]]];
            
            scalarcombinedclosureEQset[[i, 1, 1]] = scalarcombinedclosureEQset[[i, 1, 1]] /. \
              {scalarcombinedclosureEQset[[i, 1, 2]] -> scalarclosurevar};
            scalarcombinedclosureEQset[[i, 1, 2]] = scalarcombinedclosureEQset[[i, 1, 2]] /. \
              {scalarcombinedclosureEQset[[i, 1, 2]] -> scalarclosurevar};
        ];
        
        
        
        
        
        (*Combine similar vector closure variables throughout equations/BCs.*)
        movecount = 0;
        vectorcombinedclosureEQset = {};
        For[ieq = 1, ieq <= Length[term], ieq++,
            For[isection = 1, isection <= Length[term[[ieq]]], isection++,
                For[iterm = 1, iterm <= Length[term[[ieq, isection]]], iterm++,
                    If[term[[ieq, isection, iterm, 2, 3]] == 1,
                        
                        chosen = 0;
                        If[vectorcombinedclosureEQset != {},
                            For[i = 1, i <= Length[vectorcombinedclosureEQset], i++,
                                dummy = term[[ieq, isection, iterm, 2, 1]] /. \
                                  {term[[ieq, isection, iterm, 2, 2]] -> \
                                  vectorcombinedclosureEQset[[i, 1, 2]]};
                                If[vectorcombinedclosureEQset[[i, 1, 1]] == dummy,
                                    chosen = 1;
                                    vectorcombinedclosureEQset[[i, 2, ieq, isection]] += \
                                      term[[ieq, isection, iterm, 1]];
                                    vectorcombinedclosureEQset[[i, 3, ieq]] = Join[ \
                                      vectorcombinedclosureEQset[[i, 3, ieq]], \
                                      {term[[ieq, isection, iterm, 1]]}];
                                    Break[];
                                ];
                            ];
                        ];
                        
                        If[chosen == 0,
                            vectorcombinedclosureEQset = Join[vectorcombinedclosureEQset, \
                              {{{0, 0, 0}, baseequations, baseforcingterms}}];
                            
                            vectorcombinedclosureEQset[[-1, 3, ieq]] = Join[ \
                              vectorcombinedclosureEQset[[-1, 3, ieq]], \
                              {term[[ieq, isection, iterm, 1]]}];
                            
                            vectorclosurevar = ToExpression[ToString[closureprefix] <> ToString[ \
                              closurecount]];
                            vectorclosurevar = dependencyadder[vectorclosurevar, \
                            closuredependencies[[1]]];
                            closurecount += 1;
                            
                            vectorcombinedclosureEQset[[-1, 1, 1]] = \
                              term[[ieq, isection, iterm, 2, 1]] /. \
                              {term[[ieq, isection, iterm, 2, 2]] -> vectorclosurevar};
                            vectorcombinedclosureEQset[[-1, 1, 2]] = vectorclosurevar;
                            vectorcombinedclosureEQset[[-1, 1, 3]] = \
                              term[[ieq, isection, iterm, 2, 3]];
                            vectorcombinedclosureEQset[[-1, 2, ieq, isection]] += \
                              term[[ieq, isection, iterm, 1]];
                        ];
                        
                    ];
                ];
            ];
        ];
        
        
        
        
        (*Combine similar tensor closure variables throughout equations/BCs.*)
        movecount = 0;
        tensorcombinedclosureEQset = {};
        For[ieq = 1, ieq <= Length[term], ieq++,
            For[isection = 1, isection <= Length[term[[ieq]]], isection++,
                For[iterm = 1, iterm <= Length[term[[ieq, isection]]], iterm++,
                    If[term[[ieq, isection, iterm, 2, 3]] == 2,
                        
                        chosen = 0;
                        If[tensorcombinedclosureEQset != {},
                            For[i = 1, i <= Length[tensorcombinedclosureEQset], i++,
                                dummy = term[[ieq, isection, iterm, 2, 1]] /. \
                                  {term[[ieq, isection, iterm, 2, 2]] -> \
                                  tensorcombinedclosureEQset[[i, 1, 2]]};
                                If[tensorcombinedclosureEQset[[i, 1, 1]] == dummy,
                                    chosen = 1;
                                    tensorcombinedclosureEQset[[i, 2, ieq, isection]] += \
                                      term[[ieq, isection, iterm, 1]];
                                    tensorcombinedclosureEQset[[i, 3, ieq]] = Join[ \
                                      tensorcombinedclosureEQset[[i, 3, ieq]], \
                                      {term[[ieq, isection, iterm, 1]]}];
                                    Break[];
                                ];
                            ];
                        ];
                        
                        If[chosen == 0,
                            tensorcombinedclosureEQset = Join[tensorcombinedclosureEQset, \
                              {{{0, 0, 0}, baseequations, baseforcingterms}}];
                            
                            tensorcombinedclosureEQset[[-1, 3, ieq]] = Join[ \
                              tensorcombinedclosureEQset[[-1, 3, ieq]], \
                              {term[[ieq, isection, iterm, 1]]}];
                            
                            tensorclosurevar = ToExpression[ToString[closureprefix] <> ToString[ \
                              closurecount]];
                            tensorclosurevar = dependencyadder[tensorclosurevar, \
                            closuredependencies[[1]]];
                            closurecount += 1;
                            
                            tensorcombinedclosureEQset[[-1, 1, 1]] = \
                              term[[ieq, isection, iterm, 2, 1]] /. \
                              {term[[ieq, isection, iterm, 2, 2]] -> tensorclosurevar};
                            tensorcombinedclosureEQset[[-1, 1, 2]] = tensorclosurevar;
                            tensorcombinedclosureEQset[[-1, 1, 3]] = \
                              term[[ieq, isection, iterm, 2, 3]];
                            tensorcombinedclosureEQset[[-1, 2, ieq, isection]] += \
                              term[[ieq, isection, iterm, 1]];
                        ];
                        
                    ];
                ];
            ];
        ];
        
        
        dimlesscoef = Join[scalarcombinedclosureEQset, vectorcombinedclosureEQset];
        dimlesscoef = Join[dimlesscoef, tensorcombinedclosureEQset];
        
        Return[dimlesscoef];
    ];


ConsolidateClosureForms[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, \
scalars_, vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, \
dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, \
fastIV_, option_] :=
    Block[{dimlesscoef = {}, isection, iterm, iform, chosen = 0, oneoptionRank = {}, i, \
    oneoptionSecTerm = {}, multioptionRank = {}, multioptionSecTerm = {}, chosen2 = 0, \
    scalarpresent = 0, vectorpresent = 0, tensorpresent = 0, solformtermoptionSecTerm = {}, \
    solformtermoptionRank = {}, ii, term2 = term, multioptionForm = {}, fullterm, closeform, \
    iii, vectormultioptionSecTerm = {}, vectormultioptionForm = {}, \
    vectoroneoptionSecTerm = {}, chosen3 = 0, oneoptionterm, j, \
    oneoptiontermCloseVar, comparedterm, comparedtermCloseVar, comparedterm2, chosencheck = 0, \
    tensormultioptionSecTerm = {}, tensormultioptionForm = {}, tensoroneoptionSecTerm = {}, \
    flag1 = 0, scalarmultioptionSecTerm = {}, scalarmultioptionForm = {}},
        
        For[isection = 1, isection <= Length[term], isection++,
            For[iterm = 1, iterm <= Length[term[[isection]]], iterm++,
                chosen = 0;
                
                If[term[[isection, iterm, 1]] == 0 && Length[term[[isection, iterm]]] == 2,
                    chosen = 1;
                ];
                
                If[chosen == 0 && Length[term[[isection, iterm]]] == 2,
                    If[term[[isection, iterm, 2]] == {0, 0, 0},
                        chosen = 1;
                        solformtermoptionSecTerm = Join[solformtermoptionSecTerm, \
                          {{isection, iterm}}];
                    ];
                    If[chosen == 0,
                        chosen = 1;
                        oneoptionSecTerm = Join[oneoptionSecTerm, {{isection, iterm}}];
                        oneoptionRank = Join[oneoptionRank, \
                          {{term[[isection, iterm, 2, 3]]}}];
                    ];
                ];
                
                If[chosen == 0 && Length[term[[isection, iterm]]] > 2,
                    chosen = 1;
                    
                    multioptionSecTerm = Join[multioptionSecTerm, {{isection, iterm}}];
                    multioptionRank = Join[multioptionRank, {{}}];
                    multioptionForm = Join[multioptionForm, {{}}];
                    
                    For[iform = 2, iform <= Length[term[[isection, iterm]]], iform++,
                        multioptionRank[[-1]] = Join[multioptionRank[[-1]], \
                          {term[[isection, iterm, iform, 3]]}];
                        multioptionForm[[-1]] = Join[multioptionForm[[-1]], {iform}];
                    ];
                ];
                
            ];
        ];
        
        
        scalarpresent = 0;
        vectorpresent = 0;
        tensorpresent = 0;
        For[i = 1, i <= Length[oneoptionRank], i++,
            If[oneoptionRank[[i, 1]] == 0,
                scalarpresent = 1;
            ];
            If[oneoptionRank[[i, 1]] == 1,
                vectorpresent = 1;
            ];
            If[oneoptionRank[[i, 1]] == 2,
                tensorpresent = 1;
            ];
        ];
        
        
        
        
        If[vectorpresent == 1,
            
            (*Get the vectors from oneoption*)
            vectoroneoptionSecTerm = {};
            For[i = 1, i <= Length[oneoptionRank], i++,
                If[oneoptionRank[[i, 1]] == 1,
                    vectoroneoptionSecTerm = Join[vectoroneoptionSecTerm, \
                      {oneoptionSecTerm[[i]]}];
                ];
            ];
            
            
            (*Compare vectors in oneoption*)
            For[i = 1, i <= Length[vectoroneoptionSecTerm] - 1, i++,
                oneoptionterm = term[[vectoroneoptionSecTerm[[i, 1]], \
                  vectoroneoptionSecTerm[[i, 2]], 2, 1]];
                oneoptiontermCloseVar = term[[vectoroneoptionSecTerm[[i, 1]], \
                  vectoroneoptionSecTerm[[i, 2]], 2, 2]];
                For[ii = i + 1, ii <= Length[vectoroneoptionSecTerm], ii++,
                    
                    comparedterm = term[[vectoroneoptionSecTerm[[ii, 1]], \
                      vectoroneoptionSecTerm[[ii, 2]], 2, 1]];
                    comparedtermCloseVar = term[[vectoroneoptionSecTerm[[ii, 1]], \
                      vectoroneoptionSecTerm[[ii, 2]], 2, 2]];
                    comparedterm2 = comparedterm /. {comparedtermCloseVar -> \
                      oneoptiontermCloseVar};
                    
                    If[comparedterm2 == oneoptionterm,
                        chosencheck = 0;
                        For[j = 1, j <= Length[oneoptionSecTerm], j++,
                            If[oneoptionSecTerm[[j]] == vectoroneoptionSecTerm[[ii]],
                                chosencheck = 1;
                                oneoptionSecTerm = Delete[oneoptionSecTerm, j];
                                oneoptionRank = Delete[oneoptionRank, j];
                                Break[];
                            ];
                        ];
                        If[chosencheck == 0,
                            Print["CRITICAL ERROR: ConsolidateClosureForms: Something bad has happened."];
                            Return[Null];
                        ];
                        
                        Break[];
                    ];
                    If[Length[oneoptionSecTerm] == 0,
                        Print["CRITICAL ERROR: ConsolidateClosureForms: All oneoptions got deleted for vectors... This shouldnt happen..."];
                        Return[Null];
                    ];
                ];
                If[Length[oneoptionSecTerm] == 0,
                    Print["CRITICAL ERROR: ConsolidateClosureForms: All oneoptions got deleted for vectors... This shouldnt happen..."];
                    Return[Null];
                ];
            ];
            
        ];
        
        
        
        If[tensorpresent == 1,
            
            (*Get the vectors from oneoption*)
            tensoroneoptionSecTerm = {};
            For[i = 1, i <= Length[oneoptionRank], i++,
                If[oneoptionRank[[i, 1]] == 2,
                    tensoroneoptionSecTerm = Join[tensoroneoptionSecTerm, \
                      {oneoptionSecTerm[[i]]}];
                ];
            ];
            
            
            (*Compare tensors in oneoption*)
            For[i = 1, i <= Length[tensoroneoptionSecTerm] - 1, i++,
                oneoptionterm = term[[tensoroneoptionSecTerm[[i, 1]], \
                  tensoroneoptionSecTerm[[i, 2]], 2, 1]];
                oneoptiontermCloseVar = term[[tensoroneoptionSecTerm[[i, 1]], \
                  tensoroneoptionSecTerm[[i, 2]], 2, 2]];
                For[ii = i + 1, ii <= Length[tensoroneoptionSecTerm], ii++,
                    
                    comparedterm = term[[tensoroneoptionSecTerm[[ii, 1]], \
                      tensoroneoptionSecTerm[[ii, 2]], 2, 1]];
                    comparedtermCloseVar = term[[tensoroneoptionSecTerm[[ii, 1]], \
                      tensoroneoptionSecTerm[[ii, 2]], 2, 2]];
                    comparedterm2 = comparedterm /. {comparedtermCloseVar -> \
                      oneoptiontermCloseVar};
                    
                    If[comparedterm2 == oneoptionterm,
                        chosencheck = 0;
                        For[j = 1, j <= Length[oneoptionSecTerm], j++,
                            If[oneoptionSecTerm[[j]] == tensoroneoptionSecTerm[[ii]],
                                chosencheck = 1;
                                oneoptionSecTerm = Delete[oneoptionSecTerm, j];
                                oneoptionRank = Delete[oneoptionRank, j];
                                Break[];
                            ];
                        ];
                        If[chosencheck == 0,
                            Print["CRITICAL ERROR: ConsolidateClosureForms: Something bad has happened."];
                            Return[Null];
                        ];
                        
                        Break[];
                    ];
                    If[Length[oneoptionSecTerm] == 0,
                        Print["CRITICAL ERROR: ConsolidateClosureForms: All oneoptions got deleted for tensors... This shouldnt happen..."];
                        Return[Null];
                    ];
                ];
                If[Length[oneoptionSecTerm] == 0,
                    Print["CRITICAL ERROR: ConsolidateClosureForms: All oneoptions got deleted for tensors... This shouldnt happen..."];
                    Return[Null];
                ];
            ];
            
        ];
        
        
        
        
        
        
        If[Length[multioptionRank] > 0 && vectorpresent == 1,
            
            (*Get the vectors from multioption and oneoption*)
            vectormultioptionSecTerm = {};
            vectormultioptionForm = {};
            vectoroneoptionSecTerm = {};
            For[i = 1, i <= Length[multioptionRank], i++,
                vectormultioptionSecTerm = Join[vectormultioptionSecTerm, \
                  {multioptionSecTerm[[i]]}];
                vectormultioptionForm = Join[vectormultioptionForm, {{}}];
                For[ii = 1, ii <= Length[multioptionRank[[i]]], ii++,
                    If[multioptionRank[[i, ii]] == 1,
                        vectormultioptionForm[[-1]] = Join[vectormultioptionForm[[-1]], \
                          {multioptionForm[[i, ii]]}];
                    ];
                ];
                If[vectormultioptionForm[[-1]] == {},
                    vectormultioptionSecTerm = Delete[vectormultioptionSecTerm, -1];
                    vectormultioptionForm = Delete[vectormultioptionForm, -1];
                ];
            ];
            
            For[i = 1, i <= Length[oneoptionRank], i++,
                If[oneoptionRank[[i, 1]] == 1,
                    vectoroneoptionSecTerm = Join[vectoroneoptionSecTerm, \
                      {oneoptionSecTerm[[i]]}];
                ];
            ];
            
            
            (*Compare vectors in oneoption to vectors in mulioption*)
            flag1 = 0;
            For[i = 1, i <= Length[vectoroneoptionSecTerm], i++,
                oneoptionterm = term[[vectoroneoptionSecTerm[[i, 1]], \
                  vectoroneoptionSecTerm[[i, 2]], 2, 1]];
                oneoptiontermCloseVar = term[[vectoroneoptionSecTerm[[i, 1]], \
                  vectoroneoptionSecTerm[[i, 2]], 2, 2]];
                For[ii = 1, ii <= Length[vectormultioptionSecTerm], ii++,
                    For[iii = 1, iii <= Length[vectormultioptionForm[[ii]]], iii++,
                        
                        comparedterm = term[[vectormultioptionSecTerm[[ii, 1]], \
                          vectormultioptionSecTerm[[ii, 2]], \
                          vectormultioptionForm[[ii, iii]], 1]];
                        comparedtermCloseVar = term[[vectormultioptionSecTerm[[ii, 1]], \
                          vectormultioptionSecTerm[[ii, 2]], \
                          vectormultioptionForm[[ii, iii]], 2]];
                        comparedterm2 = comparedterm /. {comparedtermCloseVar -> \
                          oneoptiontermCloseVar};
                        
                        If[comparedterm2 == oneoptionterm,
                            fullterm = term[[vectormultioptionSecTerm[[ii, 1]], \
                              vectormultioptionSecTerm[[ii, 2]], 1]];
                            closeform = term[[vectormultioptionSecTerm[[ii, 1]], \
                              vectormultioptionSecTerm[[ii, 2]], \
                              vectormultioptionForm[[ii, iii]]]];
                            term2[[vectormultioptionSecTerm[[ii, 1]], \
                              vectormultioptionSecTerm[[ii, 2]]]] = \
                              {fullterm, closeform};
                            
                            chosencheck = 0;
                            For[j = 1, j <= Length[multioptionSecTerm], j++,
                                If[multioptionSecTerm[[j]] == vectormultioptionSecTerm[[ii]],
                                    chosencheck = 1;
                                    multioptionSecTerm = Delete[multioptionSecTerm, j];
                                    multioptionRank = Delete[multioptionRank, j];
                                    multioptionForm = Delete[multioptionForm, j];
                                    Break[];
                                ];
                            ];
                            If[chosencheck == 0,
                                Print["CRITICAL ERROR: ConsolidateClosureForms: Something bad has happened."];
                                Return[Null];
                            ];
                            
                            Break[];
                        ];
                    ];
                    If[Length[multioptionSecTerm] == 0,
                        flag1 = 1;
                        Break[];
                    ];
                ];
                If[Length[multioptionSecTerm] == 0 && flag1 == 1,
                    Break[];
                ];
            ];
            
        ];
        
        
        
        
        
        If[Length[multioptionRank] > 0 && tensorpresent == 1,
            
            (*Get the vectors from multioption and oneoption*)
            tensormultioptionSecTerm = {};
            tensormultioptionForm = {};
            tensoroneoptionSecTerm = {};
            For[i = 1, i <= Length[multioptionRank], i++,
                tensormultioptionSecTerm = Join[tensormultioptionSecTerm, \
                  {multioptionSecTerm[[i]]}];
                tensormultioptionForm = Join[tensormultioptionForm, {{}}];
                For[ii = 1, ii <= Length[multioptionRank[[i]]], ii++,
                    If[multioptionRank[[i, ii]] == 2,
                        tensormultioptionForm[[-1]] = Join[tensormultioptionForm[[-1]], \
                          {multioptionForm[[i, ii]]}];
                    ];
                ];
                If[tensormultioptionForm[[-1]] == {},
                    tensormultioptionSecTerm = Delete[tensormultioptionSecTerm, -1];
                    tensormultioptionForm = Delete[tensormultioptionForm, -1];
                ];
            ];
            
            For[i = 1, i <= Length[oneoptionRank], i++,
                If[oneoptionRank[[i, 1]] == 2,
                    tensoroneoptionSecTerm = Join[tensoroneoptionSecTerm, \
                      {oneoptionSecTerm[[i]]}];
                ];
            ];
            
            
            (*Compare tensors in oneoption to tensors in mulioption*)
            flag1 = 0;
            For[i = 1, i <= Length[tensoroneoptionSecTerm], i++,
                oneoptionterm = term[[tensoroneoptionSecTerm[[i, 1]], \
                  tensoroneoptionSecTerm[[i, 2]], 2, 1]];
                oneoptiontermCloseVar = term[[tensoroneoptionSecTerm[[i, 1]], \
                  tensoroneoptionSecTerm[[i, 2]], 2, 2]];
                For[ii = 1, ii <= Length[tensormultioptionSecTerm], ii++,
                    For[iii = 1, iii <= Length[tensormultioptionForm[[ii]]], iii++,
                        
                        comparedterm = term[[tensormultioptionSecTerm[[ii, 1]], \
                          tensormultioptionSecTerm[[ii, 2]], \
                          tensormultioptionForm[[ii, iii]], 1]];
                        comparedtermCloseVar = term[[tensormultioptionSecTerm[[ii, 1]], \
                          tensormultioptionSecTerm[[ii, 2]], \
                          tensormultioptionForm[[ii, iii]], 2]];
                        comparedterm2 = comparedterm /. {comparedtermCloseVar -> \
                          oneoptiontermCloseVar};
                        
                        If[comparedterm2 == oneoptionterm,
                            fullterm = term[[tensormultioptionSecTerm[[ii, 1]], \
                              tensormultioptionSecTerm[[ii, 2]], 1]];
                            closeform = term[[tensormultioptionSecTerm[[ii, 1]], \
                              tensormultioptionSecTerm[[ii, 2]], \
                              tensormultioptionForm[[ii, iii]]]];
                            term2[[tensormultioptionSecTerm[[ii, 1]], \
                              tensormultioptionSecTerm[[ii, 2]]]] = \
                              {fullterm, closeform};
                            
                            chosencheck = 0;
                            For[j = 1, j <= Length[multioptionSecTerm], j++,
                                If[multioptionSecTerm[[j]] == tensormultioptionSecTerm[[ii]],
                                    chosencheck = 1;
                                    multioptionSecTerm = Delete[multioptionSecTerm, j];
                                    multioptionRank = Delete[multioptionRank, j];
                                    multioptionForm = Delete[multioptionForm, j];
                                    Break[];
                                ];
                            ];
                            If[chosencheck == 0,
                                Print["CRITICAL ERROR: ConsolidateClosureForms: Something bad has happened."];
                                Return[Null];
                            ];
                            
                            Break[];
                        ];
                    ];
                    If[Length[multioptionSecTerm] == 0,
                        flag1 = 1;
                        Break[];
                    ];
                ];
                If[Length[multioptionSecTerm] == 0 && flag1 == 1,
                    Break[];
                ];
            ];
            
        ];
        
        
        
        
        
        
        
        
        
        
        If[scalarpresent == 1,
            
            (*Get the scalars from multioption and oneoption*)
            scalarmultioptionSecTerm = {};
            scalarmultioptionForm = {};
            For[i = 1, i <= Length[multioptionRank], i++,
                scalarmultioptionSecTerm = Join[scalarmultioptionSecTerm, \
                  {multioptionSecTerm[[i]]}];
                scalarmultioptionForm = Join[scalarmultioptionForm, {{}}];
                For[ii = 1, ii <= Length[multioptionRank[[i]]], ii++,
                    If[multioptionRank[[i, ii]] == 0,
                        scalarmultioptionForm[[-1]] = Join[scalarmultioptionForm[[-1]], \
                          {multioptionForm[[i, ii]]}];
                    ];
                ];
                If[scalarmultioptionForm[[-1]] == {},
                    scalarmultioptionSecTerm = Drop[scalarmultioptionSecTerm, -1];
                    scalarmultioptionForm = Drop[scalarmultioptionForm, -1];
                ];
            ];
            
            For[i = 1, i <= Length[scalarmultioptionSecTerm], i++,
                For[ii = 1, ii <= Length[scalarmultioptionForm[[i]]], ii++,
                    fullterm = term[[scalarmultioptionSecTerm[[i, 1]], \
                      scalarmultioptionSecTerm[[i, 2]], 1]];
                    closeform = term[[scalarmultioptionSecTerm[[i, 1]], \
                      scalarmultioptionSecTerm[[i, 2]], scalarmultioptionForm[[i, ii]]]];
                    term2[[scalarmultioptionSecTerm[[i, 1]], scalarmultioptionSecTerm[[i, 2]]]] = \
                      {fullterm, closeform};
                    
                    chosencheck = 0;
                    For[j = 1, j <= Length[multioptionSecTerm], j++,
                        If[multioptionSecTerm[[j]] == scalarmultioptionSecTerm[[i]],
                            chosencheck = 1;
                            multioptionSecTerm = Delete[multioptionSecTerm, j];
                            multioptionRank = Delete[multioptionRank, j];
                            multioptionForm = Delete[multioptionForm, j];
                            Break[];
                        ];
                    ];
                    If[chosencheck == 0,
                        Print["CRITICAL ERROR: ConsolidateClosureForms: Something bad has happened."];
                        Return[Null];
                    ];
                ];
            ];
        ];
        
        
        
        
        
        
        
        If[Length[multioptionRank] > 0,
            Print["CRITICAL ERROR: ConsolidateClosureForms: Not all multioption's have been given a chosen form to use. Write more code. The leftover multioptions are:"];
            For[i = 1, i <= Length[multioptionSecTerm], i++,
                Print[term[[multioptionSecTerm[[i, 1]], multioptionSecTerm[[i, 2]]]]];
            ];
            Return[Null];
        ];
        
        dimlesscoef = term2;
        
        Return[dimlesscoef];
    ];


closureformwriter[closureprefix_, closuredependencies_, ScalVecTen_, operator_, otherterm_] :=
    Block[{newclosureform, newclosurevar},
        
        newclosurevar = ToExpression[ToString[ \
          closureprefix[[solformtermindexglobal]]] <> ToString[closureindexglobal]];
        newclosurevar = dependencyadder[newclosurevar, closuredependencies[[ \
          solformtermindexglobal]]];
        
        closureindexglobal += 1;
        
        
        If[ScalVecTen == 0,
            newscalars = Join[newscalars, {newclosurevar}];
        ];
        If[ScalVecTen == 1,
            newvectors = Join[newvectors, {newclosurevar}];
        ];
        If[ScalVecTen == 2,
            newtensors = Join[newtensors, {newclosurevar}];
        ];
        
        
        If[operator == {},
            If[otherterm == {},
                newclosureform = newclosurevar;
                ,
                newclosureform = newclosurevar * otherterm;
                ,
                newclosureform = newclosurevar * otherterm;
            ];
            ,
            newclosureform = OperatorApplier[newclosurevar, operator, otherterm];
            ,
            Print["CRITICAL ERROR: closureformwriter: Unidentified closure operator."];
            Return[Null];
        ];
        
        Return[{newclosureform, newclosurevar, ScalVecTen}];
    ];


caseNonListmain[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, \
vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
option_] :=
    Block[{i, solution = {}, solutionvec, solformtermindexglobal, closureindexglobal, \
    newscalars, newvectors, newtensors},
        
        solformtermindexglobal = solformterm[[2]];
        closureindexglobal = closuredependencies[[2]];
        newscalars = scalars;
        newvectors = vectors;
        newtensors = tensors;
        
        If[Head[term] == Plus,
            solutionvec = ConstantArray[0, Length[term]];
            For[i = 1, i <= Length[term], i++,
                solutionvec[[i]] = casePlusSubstituter[term[[i]], solformterm[[1]], closureprefix, \
                  closuredependencies[[1]], alldependencies, newscalars, newvectors, \
                  newtensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                  fastIV, option];
                solution = Join[solution, solutionvec[[i]]];
            ];
            ,
            solution = casePlusSubstituter[term, solformterm[[1]], closureprefix, closuredependencies[[1]], \
              alldependencies, newscalars, newvectors, newtensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, option];
            ,
            solution = casePlusSubstituter[term, solformterm[[1]], closureprefix, closuredependencies[[1]], \
              alldependencies, newscalars, newvectors, newtensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, option];
        ];
        
        Return[{closureindexglobal, solution}];
    ];


casePlusSubstituter[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, \
vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
option_] :=
    Block[{solution, levelcountdummy = 0, i, newterm, newterm2, coupledindex, chosen = 0},
        
        If[Head[term] == Plus,
            Print["CRITICAL ERROR: casePlusSubstituter: Term came in as a Plus. Not supposed to happen. Term is: ", term];
            Return[Null];
        ];
        
        
        
        chosen = 0;
        
        (*See if the dependent variable you are solving for (corresponding to the equation) is
        in the term.*)
        newterm = term /. {solformterm[[solformtermindexglobal]] -> 0};
        newterm = getterminfo[newterm, {}, scalars, vectors, tensors, closurevar, \
          graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
          gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, {}];
        
        newterm = MakeSingleTerm[newterm, {}, dotdenoters, tensorproductdenoters, \
          doubledotdenoters];
        
        If[newterm[[1, 1, 1]] == 0,
            chosen = 1;
            solution = {{term, {0, 0, 0}}};
            
            Return[solution];
        ];    
        
        
        
        coupledindex = {};
        For[i = 1, i <= Length[solformterm], i++,
            newterm2 = term /. {solformterm[[i]] -> 0};
            newterm2 = getterminfo[newterm2, {}, scalars, vectors, tensors, closurevar, \
              graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
              gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, {}];
            
            newterm2 = MakeSingleTerm[newterm2, {}, dotdenoters, tensorproductdenoters, \
              doubledotdenoters];
              
            If[newterm2[[1, 1, 1]] == 0,
                coupledindex = Join[coupledindex, {i}];
            ];
        ];
        
        If[coupledindex != {},
            chosen = 1;
            solution = {{term, {0, 0, 0}}};
            
            Return[solution];
        ];
        
        
        
        If[chosen == 0,
            solution = decider[newterm[[1, 1, 1]], solformterm, closureprefix, closuredependencies, \
              alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, levelcountdummy, {}];
            
            Return[solution];
        ];
        
    ];


decider[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
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
                dimlesscoef = caseList[term, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Rational && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Rational"];                
                ];
                chosen = 1;
                dimlesscoef = caseRational[term, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Integer && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Integer"];                
                ];
                chosen = 1;
                dimlesscoef = caseInteger[term, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Symbol && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Symbol"];                
                ];
                chosen = 1;
                dimlesscoef = caseSymbol[term, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[pietrzykflipdenoters[[1]]] && chosen == 0,
                If[term[[2]] == pietrzykflipdenoters[[2]],
                    If[printouttoggle == 1,
                        Print["getterminfo: Identified Pietrzyk Flip"];                
                    ];
                    chosen = 1;
                    dimlesscoef = casePietrzykFlip[term, solformterm, closureprefix, closuredependencies, \
                      alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                      divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                      gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                ];
            ];
            If[Head[term] == Power && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Power"];                
                ];
                chosen = 1;
                dimlesscoef = casePower[term, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[derivativedenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Derivative"];                
                ];
                chosen = 1;
                dimlesscoef = caseDerivative[term, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Times && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Times"];                
                ];
                chosen = 1;
                dimlesscoef = caseTimes[term, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[dotdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Dot"];                
                ];
                chosen = 1;
                dimlesscoef = caseDot[term, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[tensorproductdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified TensorProduct"];                
                ];
                chosen = 1;
                dimlesscoef = caseTensorProduct[term, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[doubledotdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified DoubleDot"];                
                ];
                chosen = 1;
                dimlesscoef = caseDoubleDot[term, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Plus && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Plus"];                
                ];
                chosen = 1;
                dimlesscoef = casePlus[term, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[graddenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Gradient"];
                ];
                chosen = 1;
                dimlesscoef = caseGradient[term, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[divdenoters[[1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Divergence"];
                ];
                chosen = 1;
                dimlesscoef = caseDivergence[term, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            ];
            If[Head[term] == Head[avgdenoters[[1, 1]]] && chosen == 0,
                If[printouttoggle == 1,
                    Print["decider: Identified Average"];
                ];
                chosen = 1;
                dimlesscoef = caseAverage[term, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
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
                dimlesscoef = decider[Head[term], solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
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
caseList[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{},
        Print["CRITICAL ERROR: caseList: Not written yet (but there shouldnt be any lists). Term is: ", term];
        Return[Null];
    ];


(*Ready*)
caseRational[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef},
        
        dimlesscoef = {{term, closureformwriter[closureprefix, closuredependencies, 0, \
          {}, 1]}};
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseInteger[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef},
        
        If[term == 0,
            Return[{{0, {0, 0, 0}}}];
        ];
        
        dimlesscoef = {{term, closureformwriter[closureprefix, closuredependencies, 0, \
          {}, 1]}};
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseSymbol[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, tensors_, \
closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, term2, argumentparts, argumentparts2, chosen = 0},
        
        If[option == {},
            
            dimlesscoef = {{term, closureformwriter[closureprefix, closuredependencies, 0, \
              {}, 1]}};
            
            ,
            
            term2 = dependencyadder[term, option];
            argumentparts = getterminfo[term2, closuredependencies[[solformtermindexglobal]], \
              scalars, vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
              dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
              slowIV, fastIV, levelcountdummy, option];
            
            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            
            chosen = 0;
            If[argumentparts[[1, 3, 1]] == 0,
                chosen = 1;
                dimlesscoef = {{term2, closureformwriter[closureprefix, closuredependencies, \
                  0, {}, term2]}};
            ];
            If[chosen == 0 && argumentparts[[1, 1, 1]] == \
              solformterm[[solformtermindexglobal]],
                chosen = 1;
                dimlesscoef = {{term2, {0, 0, 0}}};
            ];
            If[chosen == 0 && argumentparts[[1, 3, 1]] == 1,
                argumentparts2 = getterminfo[term2, Complement[alldependencies, \
                  closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                  fastIV, levelcountdummy, option];
                argumentparts2 = MakeSingleTerm[argumentparts2, {}, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters];
                If[argumentparts2[[1, 3, 1]] == 0,
                    chosen = 1;
                    dimlesscoef = {{term2, closureformwriter[closureprefix, closuredependencies, \
                      argumentparts2[[1, 2, 1]], {}, 1]}};
                ];
            ];
            
            If[chosen == 0,
                Print["CRITICAL ERROR: caseSymbol: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                Return[Null];
            ];
            
            ,
            
            Print["CRITICAL ERROR: caseSymbol: variable 'option' was not identified. Term was: ", term];
            Return[Null];
            
        ];
        
        Return[dimlesscoef];
    ];
    


(*Ready*)
casePietrzykFlip[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef, argument = term[[1]], superscript = term[[2]], inthepower, inthepower2, \
    i, expandedargument, chosen = 0, dummy, dimlesscoefparts, bracketeddummy},
        
        expandedargument = Expand[argument];
        
        If[Head[expandedargument] == Plus,
            dimlesscoefparts = {};
            dimlesscoef = {};
            For[i = 1, i <= Length[expandedargument], i++,
                bracketeddummy = OperatorApplier[expandedargument[[i]], pietrzykflipdenoters, \
                  superscript];
                dimlesscoefparts = decider[bracketeddummy, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                dimlesscoef = Join[dimlesscoef, dimlesscoefparts];
            ];
            
            Return[dimlesscoef];
        ];
        
        
        
        
        
        inthepower = getterminfo[argument, closuredependencies, scalars, \
          vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
          levelcountdummy, option];
        
        inthepower = MakeSingleTerm[inthepower, {}, dotdenoters, tensorproductdenoters, \
          doubledotdenoters];
        
        chosen = 0;
        If[inthepower[[1, 3, 1]] == 0 && chosen == 0,
            inthepower2 = getterminfo[inthepower[[1, 1, 1]], Complement[alldependencies, \
              closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
              fastIV, levelcountdummy, option];
            inthepower2 = MakeSingleTerm[inthepower2, {}, dotdenoters, tensorproductdenoters, \
              doubledotdenoters];
            If[inthepower2[[1, 3, 1]] == 0,
                chosen = 1;
                dimlesscoef = {{term, closureformwriter[closureprefix, closuredependencies, \
                  0, {}, 1]}};
                ,
                chosen = 1;
                dimlesscoef = {{term, closureformwriter[closureprefix, closuredependencies, \
                  0, {}, term]}};
            ];
        ];
        If[chosen == 0 && OperatorApplier[inthepower[[1, 1, 1]], pietrzykflipdenoters, \
          superscript] == solformterm[[solformtermindexglobal]],
            chosen = 1;
            dimlesscoef = {{term, {0, 0, 0}}};
        ];
        If[chosen == 0 && inthepower[[1, 3, 1]] == 1,
            inthepower2 = getterminfo[inthepower[[1, 1, 1]], Complement[alldependencies, \
              closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
              fastIV, levelcountdummy, option];
            inthepower2 = MakeSingleTerm[inthepower2, {}, dotdenoters, tensorproductdenoters, \
              doubledotdenoters];
            If[inthepower2[[1, 3, 1]] == 0,
                chosen = 1;
                dimlesscoef = {{term, closureformwriter[closureprefix, closuredependencies, \
                  inthepower2[[1, 2, 1]], {}, 1]}};
            ];
        ];
            
        If[chosen == 0,
            Print["CRITICAL ERROR: casePietrzykFlip: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
            Return[Null];
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
casePower[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef, argument = term[[1]], power = term[[2]], inthepower, inthepower2, \
    i, expandedterm = Expand[term], chosen = 0, dummy},
        
        If[Head[expandedterm] == Plus,
            dimlesscoef = {};
            For[i = 1, i <= Length[expandedterm], i++,
                dummy = decider[expandedterm[[i]], solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                dimlesscoef = Join[dimlesscoef, dummy];
            ];
            
            Return[dimlesscoef];
        ];
        
        
        
        
        
        inthepower = getterminfo[argument, closuredependencies, scalars, \
          vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
          levelcountdummy, option];
        
        inthepower = MakeSingleTerm[inthepower, {}, dotdenoters, tensorproductdenoters, \
          doubledotdenoters];
        
        chosen = 0;
        If[inthepower[[1, 3, 1]] == 0,
            inthepower2 = getterminfo[inthepower[[1, 1, 1]], Complement[alldependencies, \
              closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
              fastIV, levelcountdummy, option];
            inthepower2 = MakeSingleTerm[inthepower2, {}, dotdenoters, tensorproductdenoters, \
              doubledotdenoters];
            If[inthepower2[[1, 3, 1]] == 0,
                chosen = 1;
                dimlesscoef = {{term, closureformwriter[closureprefix, closuredependencies, \
                  0, {}, 1]}};
                ,
                chosen = 1;
                dimlesscoef = {{term, closureformwriter[closureprefix, closuredependencies, \
                  0, {}, term]}};
            ];
        ];
        If[chosen == 0 && inthepower[[1, 1, 1]]^power == \
          solformterm[[solformtermindexglobal]],
            chosen = 1;
            dimlesscoef = {{term, {0, 0, 0}}};
        ];
        If[chosen == 0 && inthepower[[1, 3, 1]] == 1,
            inthepower2 = getterminfo[inthepower[[1, 1, 1]], Complement[alldependencies, \
              closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
              fastIV, levelcountdummy, option];
            inthepower2 = MakeSingleTerm[inthepower2, {}, dotdenoters, tensorproductdenoters, \
              doubledotdenoters];
            If[inthepower2[[1, 3, 1]] == 0,
                chosen = 1;
                dimlesscoef = {{term, closureformwriter[closureprefix, closuredependencies, \
                  inthepower2[[1, 2, 1]], {}, 1]}};
            ];
        ];
            
        If[chosen == 0,
            Print["CRITICAL ERROR: casePower: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
            Return[Null];
        ];
        
        Return[dimlesscoef];
    ];


caseDerivative[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, subscript = term[[2]], argument = term[[1]], argumentparts, i, \
    expandedargument, dimlesscoefparts, bracketeddummy, chosen2 = 0, \
    newargumentparts = {{{}, {}, {}}}, flaggradientsubscript = 0, scalarmults2, \
    scalarmults = {{{}, {}, {}}}, checkdependencyandscalar, newargumentparts2},
        
        expandedargument = Expand[argument];
        
        If[Head[expandedargument] == Plus,
            dimlesscoefparts = {};
            dimlesscoef = {};
            For[i = 1, i <= Length[expandedargument], i++,
                checkdependencyandscalar = getterminfo[expandedargument[[i]], {subscript}, \
                  scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                  avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                  fastIV, levelcountdummy, option];
                  
                checkdependencyandscalar = MakeSingleTerm[checkdependencyandscalar, {}, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                
                If[checkdependencyandscalar[[1, 3, 1]] == 1,
                    bracketeddummy = OperatorApplier[expandedargument[[i]], derivativedenoters, \
                      subscript];
                    dimlesscoefparts = decider[bracketeddummy, solformterm, closureprefix, \
                      closuredependencies, alldependencies, scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    dimlesscoef = Join[dimlesscoef, dimlesscoefparts];
                ];
            ];
            
            Return[dimlesscoef];
        ];
        
        
        
        
        
        (*Check to see if Gradient is appropriate, or if you can return 0*)
        checkdependencyandscalar = getterminfo[argument, {subscript}, scalars, vectors, \
          tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
          levelcountdummy, option];
        
        checkdependencyandscalar = MakeSingleTerm[checkdependencyandscalar, {}, dotdenoters, \
          tensorproductdenoters, doubledotdenoters];
        
        If[checkdependencyandscalar[[1, 3, 1]] == 0,
            dimlesscoef = {{0, {0, 0, 0}}};
            Return[dimlesscoef];
        ];
        
        
        
        flaggradientsubscript = 0;
        If[Intersection[closuredependencies[[solformtermindexglobal]], {subscript}] != {},
            flaggradientsubscript = 1;
            
            argumentparts = getterminfo[argument, Complement[alldependencies, \
              closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
            
            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            
            If[argumentparts[[1, 3, 1]] == 0,
                dimlesscoef = {{term, closureformwriter[closureprefix, closuredependencies, \
                  argumentparts[[1, 2, 1]], {}, 1]}};
                ,
                Print["CRITICAL ERROR: caseDerivative: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                Return[Null];
            ];
        ];
        
        If[flaggradientsubscript == 0,
            
            argumentparts = getterminfo[argument, \
              closuredependencies[[solformtermindexglobal]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
            
            chosen2 = 0;
            If[Length[argumentparts] > 1,
                chosen2 = 1;
                newargumentparts = 0;
                For[i = 1, i <= Length[argumentparts], i++,
                    newargumentparts += OperatorApplier[listProduct[argumentparts[[i, 1]]], \
                      derivativedenoters, subscript];
                ];
                newargumentparts = Expand[newargumentparts];
                
                dimlesscoef = decider[newargumentparts, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                Return[dimlesscoef];
            ];
            
            If[Length[argumentparts] == 1,
                chosen2 = 1;
                newargumentparts = {{{}, {}, {}}};
                scalarmults = {{{}, {}, {}}};
                
                (*Take out the scalars that arent functions of the appropriate independent
                variable.*)
                For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
                    If[argumentparts[[1, 3, i]] == 0,
                        scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], \
                          {argumentparts[[1, 1, i]]}];
                        scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], \
                          {argumentparts[[1, 2, i]]}];
                        scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], \
                          {argumentparts[[1, 3, i]]}];
                        ,
                        newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                          {argumentparts[[1, 1, i]]}];
                        newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                          {argumentparts[[1, 2, i]]}];
                        newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                          {argumentparts[[1, 3, i]]}];
                    ];
                ];
                
                (*Handle case of no scalars w.r.t. the average*)
                If[scalarmults == {{{}, {}, {}}},
                    scalarmults = {{{1}, {0}, {0}}};
                ];
                
                (*Handle things in the average and multiply everything together*)
                If[newargumentparts == {{{}, {}, {}}},
                    
                    scalarmults2 = getterminfo[listProduct[scalarmults[[1, 1]]], Complement[alldependencies, \
                      closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    scalarmults2 = MakeSingleTerm[scalarmults2, {}, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters];
                    dimlesscoef = OperatorApplier[scalarmults2[[1, 1, 1]], \
                      derivativedenoters, subscript];
                    If[scalarmults2[[1, 3, 1]] == 1,
                        dimlesscoef = {{dimlesscoef, closureformwriter[closureprefix, \
                          closuredependencies, 0, {}, dimlesscoef]}};
                        ,
                        dimlesscoef = {{dimlesscoef, closureformwriter[closureprefix, \
                          closuredependencies, 0, {}, 1]}};
                    ];
                    
                    ,
                    
                    newargumentparts2 = getterminfo[listProduct[newargumentparts2[[1, 1]]], Complement[alldependencies, \
                      closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    newargumentparts2 = MakeSingleTerm[newargumentparts2, {}, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters];
                    
                    If[newargumentparts2[[1, 3, 1]] == 1,
                        Print["CRITICAL ERROR: caseDerivative: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                        Return[Null];
                        ,
                        Print["CRITICAL ERROR: caseDerivative: Need to write more code. Really not expecting this option... but it could occur for time averaging. Term is: ", term];
                        Return[Null];
                    ];
                    
                    ,
                    
                    Print["CRITICAL ERROR: caseDerivative: Unable to identify newargumentparts. Term is: ", term];
                    Return[Null];
                ];
            ];
            
            If[chosen2 == 0,
                Print["CRITICAL ERROR: caseDerivative: Unable to identify the number of terms coming back. Term is: ", term];
                Return[Null];
            ];
            
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseTimes[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef, expandedterm = Expand[term], i, j, scalarmults = {{{}, {}, {}}}, \
    chosen2 = 0, argumentparts, newargumentparts = {{{}, {}, {}}}, newargumentparts2, isum, \
    newargumentparts3, dimlesscoef2, scalarargumentparts = {{{}, {}, {}}}, \
    scalarmults2 = {{{}, {}, {}}}, dummy, chosen = 0},
        
        If[Head[expandedterm] == Plus,
            dimlesscoef = {};
            For[i = 1, i <= Length[expandedterm], i++,
                dummy = decider[expandedterm[[i]], solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                dimlesscoef = Join[dimlesscoef, dummy];
            ];
            
            Return[dimlesscoef];
        ];
        
        
        
        
        
        argumentparts = getterminfo[term, closuredependencies[[solformtermindexglobal]], \
          scalars, vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
          dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
          slowIV, fastIV, levelcountdummy, option];
        
        chosen2 = 0;
        If[Length[argumentparts] > 1,
            chosen2 = 1;
            newargumentparts = 0;
            For[i = 1, i <= Length[argumentparts], i++,
                newargumentparts += listProduct[argumentparts[[i, 1]]];
            ];
            newargumentparts = Expand[newargumentparts];
            dimlesscoef = decider[newargumentparts, solformterm, closureprefix, closuredependencies, \
              alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            
            Return[dimlesscoef];
        ];
        
        If[Length[argumentparts] == 1,
            chosen2 = 1;
            newargumentparts = {{{}, {}, {}}};
            scalarmults = {{{}, {}, {}}};
            
            (*Take out the scalars that arent functions of the appropriate independent variable.*)
            For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
                If[argumentparts[[1, 3, i]] == 0,
                    scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], \
                      {argumentparts[[1, 1, i]]}];
                    scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], \
                      {argumentparts[[1, 2, i]]}];
                    scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], \
                      {argumentparts[[1, 3, i]]}];
                    ,
                    newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                      {argumentparts[[1, 1, i]]}];
                    newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                      {argumentparts[[1, 2, i]]}];
                    newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                      {argumentparts[[1, 3, i]]}];
                ];
            ];
            
            (*Handle case of no scalars w.r.t. the average*)
            If[scalarmults == {{{}, {}, {}}},
                scalarmults = {{{1}, {0}, {0}}};
            ];
            
            
            (*Handle things in the average and multiply everything together*)
            If[newargumentparts == {{{}, {}, {}}},
                
                dimlesscoef = caseTimesSimp1[scalarmults, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                ,
                
                If[Length[newargumentparts[[1, 1]]] == 1,
                    newargumentparts2 = decider[newargumentparts[[1, 1, 1]], solformterm, closureprefix, \
                      closuredependencies, alldependencies, scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                      
                    dimlesscoef = newargumentparts2;
                    
                    dimlesscoef = caseTimesSimp3[scalarmults, dimlesscoef, solformterm, closureprefix, \
                      closuredependencies, alldependencies, scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                      dotdenoters, tensorproductdenoters, doubledotdenoters, \
                      pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
                    ,
                    
                    newargumentparts2 = MakeSingleTerm[newargumentparts, {}, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters];
                    newargumentparts2 = getterminfo[newargumentparts2[[1, 1, 1]], Complement[ \
                      alldependencies, closuredependencies[[solformtermindexglobal]]], \
                      scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                      avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                      normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                    
                    dimlesscoef = {};
                    For[i = 1, i <= Length[newargumentparts2], i++,
                        
                        newargumentparts3 = MakeSingleTerm[{newargumentparts2[[i]]}, {}, \
                          dotdenoters, tensorproductdenoters, doubledotdenoters];
                        
                        If[newargumentparts3[[1, 3, 1]] == 0 && chosen == 0,
                            dimlesscoef = Join[dimlesscoef, {{newargumentparts3[[1, 1, 1]] * \
                              listProduct[scalarmults[[1, 1]]], closureformwriter[closureprefix, \
                              closuredependencies, newargumentparts3[[1, 2, 1]], {}, \
                              listProduct[scalarmults[[1, 1]]]]}}];
                            ,
                            dimlesscoef2 = caseTimesSimp2[{newargumentparts2[[i]]}, solformterm, closureprefix, \
                              closuredependencies, alldependencies, scalars, vectors, tensors, \
                              closurevar, graddenoters, divdenoters, avgdenoters, \
                              derivativedenoters, dotdenoters, tensorproductdenoters, \
                              doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                              normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                            dimlesscoef = Join[dimlesscoef, dimlesscoef2];
                        ];
                        
                    ];
                    
                    ,
                    
                    Print["CRITICAL ERROR: caseTimes: Unable to identify newargumentparts. Term is: ", term];
                    Return[Null];
                ];
                
                ,
                
                Print["CRITICAL ERROR: caseTimes: Unable to identify newargumentparts. Term is: ", term];
                Return[Null];
            ];
            
            ,
            
            Print["CRITICAL ERROR: caseTimes: Unable to identify argumentparts. Term is: ", term];
            Return[Null];
        ];
        
        Return[dimlesscoef];
    ];


caseTimesSimp3[term_, dimlesscoefprev_, solformterm_, closureprefix_, closuredependencies_, \
alldependencies_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, \
avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{i, ii, iii, argumentparts, dimlesscoef = dimlesscoefprev, newargumentparts1, newargumentparts2, \
    scalarmults = {{{}, {}, {}}}, newargumentparts = {{{}, {}, {}}}, \
    currentclosvar = dimlesscoefprev[[1, 2, 2]]},
        
        argumentparts = getterminfo[listProduct[term[[1, 1]]], Complement[alldependencies, \
          closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
          levelcountdummy, option];
        
        If[Length[argumentparts] > 1,
            dimlesscoef[[1, 1]] = dimlesscoef[[1, 1]] * listProduct[term[[1, 1]]];
            For[i = 2, i <= Length[dimlesscoef[[1]]], i++,
                dimlesscoef[[1, i, 1]] = dimlesscoef[[1, i, 1]] * listProduct[term[[1, 1]]];
            ];
            Print["Warning: caseTimesSimp3: Could not run caseTimesSimp3. Analysis of scalar term came back as a sum. Scalar term is: ", term];
            Return[dimlesscoef];
        ];
        
        newargumentparts = {{{}, {}, {}}};
        scalarmults = {{{}, {}, {}}};
        For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
            If[argumentparts[[1, 3, i]] == 0,
                scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], \
                  {argumentparts[[1, 1, i]]}];
                scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], \
                  {argumentparts[[1, 2, i]]}];
                scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], \
                  {argumentparts[[1, 3, i]]}];
                ,
                newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                  {argumentparts[[1, 1, i]]}];
                newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                  {argumentparts[[1, 2, i]]}];
                newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                  {argumentparts[[1, 3, i]]}];
            ];
        ];
        
        (*Handle case of all constants (w.r.t. any independent variables)*)
        If[newargumentparts == {{{}, {}, {}}},
            dimlesscoef[[1, 1]] = dimlesscoef[[1, 1]] * listProduct[term[[1, 1]]];
            ,
            dimlesscoef[[1, 1]] = dimlesscoef[[1, 1]] * listProduct[term[[1, 1]]];
            For[i = 2, i <= Length[dimlesscoef[[1]]], i++,
                dimlesscoef[[1, i, 1]] = dimlesscoef[[1, i, 1]] * \
                  listProduct[newargumentparts[[1, 1]]];
            ];
            ,
            dimlesscoef[[1, 1]] = dimlesscoef[[1, 1]] * listProduct[term[[1, 1]]];
            For[i = 2, i <= Length[dimlesscoef[[1]]], i++,
                dimlesscoef[[1, i, 1]] = dimlesscoef[[1, i, 1]] * \
                  listProduct[newargumentparts[[1, 1]]];
            ];
        ];
        
        Return[dimlesscoef];
    ];


caseTimesSimp2[term_, solformterm_, closureprefix_, closuredependencies_, \
alldependencies_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, \
avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, \
pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef, iterms = 1, chosen = 0, flagdot = 0, flagnotdot = 0, termdot, \
    termnotdot, i, newtermnotdot, termdotunfold, inavg, outavg, success, dependentIndices, \
    termdotunfold2, chosen3, infloopstop, groupeddottedcomponents, irank, \
    groupeddottedcomponentsRank, newtermnotdot2, replacevar, newargumentparts2 = {}},
        
        chosen = 0;
        
        If[Length[term[[iterms, 1]]] == 2 && chosen == 0,
            
            (*Confirm that one component of the term is a dot product and the other is not.*)
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
            
            (*For the "other" component, confirm that it is a scalar that is one component long
            and only dependents on the closure independent variables.*)
            If[flagnotdot == 1,
                newtermnotdot = getterminfo[termnotdot[[1, 1, 1]], \
                  closuredependencies[[solformtermindexglobal]], scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
                newtermnotdot2 = getterminfo[termnotdot[[1, 1, 1]], Complement[ \
                  alldependencies, closuredependencies[[solformtermindexglobal]]], scalars, \
                  vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, \
                  derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
                  pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                  fastIV, levelcountdummy, option];
                If[Length[newtermnotdot] == 1 && Length[newtermnotdot[[1, 1]]] == 1 && \
                  newtermnotdot[[1, 3, 1]] == 1 && newtermnotdot[[1, 2, 1]] == 0 && \
                  newtermnotdot2[[1, 3, 1]] == 0,
                    flagnotdot = 1;
                    ,
                    flagnotdot = 0;
                    ,
                    flagnotdot = 0;
                ];
            ];
            
            If[flagdot == 1 && flagnotdot == 1,
                
                termdotunfold = UnfoldMergedTerms[termdot, dotdenoters, \
                  closuredependencies[[solformtermindexglobal]], scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                  divform, gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                  option];
                termdotunfold2 = UnfoldMergedTerms[termdot, dotdenoters, Complement[ \
                  alldependencies, closuredependencies[[solformtermindexglobal]]], scalars, \
                  vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, \
                  derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, \
                  pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
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
                    For[i = 1, i <= Length[dependentIndices], i++,
                        If[termdotunfold2[[1, 3, 1, dependentIndices[[i]]]] == 1,
                            success = 0;
                        ];
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
                        
                        (*Find the rank of the new term.*)
                        For[irank = 1, irank <= Length[dependentIndices], irank++,
                            If[irank == 1,
                                groupeddottedcomponentsRank = termdotunfold[[1, 2, 1, \
                                  dependentIndices[[irank]]]];
                                ,
                                groupeddottedcomponentsRank = Abs[groupeddottedcomponentsRank - \
                                  termdotunfold[[1, 2, 1, dependentIndices[[irank]]]]];
                            ];
                        ];
                        termdotunfold[[1, 2, 1, dependentIndices[[1]]]] = \
                          groupeddottedcomponentsRank;
                        
                        replacevar = closureformwriter[closureprefix, \
                          closuredependencies, groupeddottedcomponentsRank, {}, 1];
                        termdotunfold[[1, 1, 1, dependentIndices[[1]]]] = replacevar[[2]];
                        
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
                        
                        termdotunfold = MergeUnfoldedTerms[termdotunfold, dotdenoters, \
                          dotdenoters, tensorproductdenoters, doubledotdenoters];
                        dimlesscoef = {{listProduct[term[[iterms, 1]]], \
                          {termdotunfold[[1, 1, 1]], replacevar[[2]], replacevar[[3]]}}};
                        
                        newargumentparts2 = Join[newargumentparts2, dimlesscoef];
                        
                    ];
                ];
                
            ];
        ];
        
        
        If[chosen == 0,
            Print["CRITICAL ERROR: caseTimesSimp2: Term encountered that relies partially on the planned dependencies of the closure variable, and partially on the unplanned independent variables (unable to separate scales). Need to write more code. term is: ", term];
            Return[Null];
        ];
        
        Return[newargumentparts2];
    ];


caseTimesSimp1[term_, solformterm_, closureprefix_, closuredependencies_, \
alldependencies_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, \
avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{i, ii, iii, argumentparts, dimlesscoef = {}, newargumentparts1, newargumentparts2, \
    scalarmults = {{{}, {}, {}}}, newargumentparts = {{{}, {}, {}}}, currentclosvar},
        
        dimlesscoef = {{listProduct[term[[1, 1]]]}};
        
        argumentparts = getterminfo[listProduct[term[[1, 1]]], Complement[alldependencies, \
          closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
          levelcountdummy, option];
        
        If[Length[argumentparts] > 1,
            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
        ];
        
        newargumentparts = {{{}, {}, {}}};
        scalarmults = {{{}, {}, {}}};
        For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
            If[argumentparts[[1, 3, i]] == 0,
                scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], \
                  {argumentparts[[1, 1, i]]}];
                scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], \
                  {argumentparts[[1, 2, i]]}];
                scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], \
                  {argumentparts[[1, 3, i]]}];
                ,
                newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                  {argumentparts[[1, 1, i]]}];
                newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                  {argumentparts[[1, 2, i]]}];
                newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                  {argumentparts[[1, 3, i]]}];
            ];
        ];
        
        
        (*Handle case of all constants (w.r.t. any independent variables)*)
        If[scalarmults == {{{}, {}, {}}},
            scalarmults = {{{1}, {0}, {0}}};
        ];
        If[newargumentparts == {{{}, {}, {}}},
            dimlesscoef[[1]] = Join[dimlesscoef[[1]], {closureformwriter[closureprefix, \
              closuredependencies, 0, {}, 1]}];
            newargumentparts = {{{1}, {0}, {0}}};
            ,
            dimlesscoef[[1]] = Join[dimlesscoef[[1]], {closureformwriter[closureprefix, \
              closuredependencies, 0, {}, listProduct[newargumentparts[[1, 1]]]]}];
            ,
            dimlesscoef[[1]] = Join[dimlesscoef[[1]], {closureformwriter[closureprefix, \
              closuredependencies, 0, {}, listProduct[newargumentparts[[1, 1]]]]}];
        ];
        
        currentclosvar = dimlesscoef[[1, 2, 2]];
        
        
        
        
        
        (*Test for potential closure forms that are dot products of constants*)
        newargumentparts1 = UnfoldMergedTerms[newargumentparts, dotdenoters, Complement[ \
          alldependencies, closuredependencies[[solformtermindexglobal]]], scalars, vectors, \
          tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
          slowIV, fastIV, levelcountdummy, option];
        
        If[Length[newargumentparts1] > 1,
            Print["Warning: caseTimesSimp1: Could not run caseTimesSimp1, as term came back as a sum. Term is: ", term];
            Return[{}];
        ];
        
        For[ii = 1, ii <= Length[newargumentparts1[[1, 1]]], ii++,
            If[Head[newargumentparts1[[1, 1, ii]]] == List,
                For[iii = 1, iii <= Length[newargumentparts1[[1, 3, ii]]], iii++,
                    If[newargumentparts1[[1, 3, ii, iii]] == 0,
                        newargumentparts2 = newargumentparts1;
                        newargumentparts2[[1, 1, ii, iii]] = currentclosvar;
                        newargumentparts2 = MakeSingleTerm[newargumentparts2, dotdenoters, \
                          dotdenoters, tensorproductdenoters, doubledotdenoters];
                        dimlesscoef[[1]] = Join[dimlesscoef[[1]], {{newargumentparts2[[1, 1, 1]], \
                          currentclosvar, newargumentparts1[[1, 2, ii, iii]]}}];
                    ];
                ];
            ];
        ];
        
        
        
        
        (*Test for potential closure forms that are double dot products of constants*)
        newargumentparts1 = UnfoldMergedTerms[newargumentparts, doubledotdenoters, Complement[ \
          alldependencies, closuredependencies[[solformtermindexglobal]]], scalars, vectors, \
          tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
          slowIV, fastIV, levelcountdummy, option];
        
        If[Length[newargumentparts1] > 1,
            Print["Warning: caseTimesSimp1: Could not run caseTimesSimp1, as term came back as a sum. Term is: ", term];
            Return[{}];
        ];
        
        For[ii = 1, ii <= Length[newargumentparts1[[1, 1]]], ii++,
            If[Head[newargumentparts1[[1, 1, ii]]] == List,
                For[iii = 1, iii <= Length[newargumentparts1[[1, 3, ii]]], iii++,
                    If[newargumentparts1[[1, 3, ii, iii]] == 0,
                        newargumentparts2 = newargumentparts1;
                        newargumentparts2[[1, 1, ii, iii]] = currentclosvar;
                        newargumentparts2 = MakeSingleTerm[newargumentparts2, doubledotdenoters, \
                          dotdenoters, tensorproductdenoters, doubledotdenoters];
                        dimlesscoef[[1]] = Join[dimlesscoef[[1]], {{newargumentparts2[[1, 1, 1]], \
                          currentclosvar, newargumentparts1[[1, 2, ii, iii]]}}];
                    ];
                ];
            ];
        ];
        
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseDot[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef, Nterms = Length[term], i, ii, j, iii, chosen2, distributeddot, \
    expandedterm, argumentparts, newargumentparts = {{{}, {}, {}}}, scalarmults = {{{}, {}, {}}}, \
    newargumentpartsUTP, dummy, dimlesscoef2, scalarmults2},
        
        For[i = 1, i <= Nterms, i++,
            If[Head[term[[i]]] == Plus,
                expandedterm = 0;
                For[ii = 1, ii <= Length[term[[i]]], ii++,
                    For[iii = 1, iii <= Nterms, iii++,
                        If[iii == 1,
                            If[iii == i,
                                distributeddot = term[[iii, ii]];
                                ,
                                distributeddot = term[[iii]];
                            ];
                            ,
                            If[iii == i,
                                distributeddot = OperatorApplier[distributeddot, dotdenoters, \
                                  term[[iii, ii]]];
                                ,
                                distributeddot = OperatorApplier[distributeddot, dotdenoters, \
                                  term[[iii]]];
                            ];
                        ];
                    ];
                    expandedterm += distributeddot;
                ];
                
                dimlesscoef = decider[expandedterm, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                Return[dimlesscoef];
            ];
        ];
        
        
        
        
        
        argumentparts = getterminfo[term, closuredependencies[[solformtermindexglobal]], \
          scalars, vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
          dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
          slowIV, fastIV, levelcountdummy, option];
        
        chosen2 = 0;
        If[Length[argumentparts] > 1 && chosen2 == 0,
            chosen2 = 1;
            newargumentparts = 0;
            For[i = 1, i <= Length[argumentparts], i++,
                newargumentparts += listProduct[argumentparts[[i, 1]]];
            ];
            newargumentparts = Expand[newargumentparts];
            
            dimlesscoef = decider[newargumentparts, solformterm, closureprefix, closuredependencies, \
              alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            
            Return[dimlesscoef];
        ];
        
        If[Length[argumentparts] == 1 && Head[argumentparts[[1, 1, 1]]] == \
          Head[doubledotdenoters[[1]]] && chosen2 == 0,
            chosen2 = 1;
                        
            dimlesscoef = decider[argumentparts[[1, 1, 1]], solformterm, closureprefix, \
              closuredependencies, alldependencies, scalars, vectors, tensors, closurevar, \
              graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, levelcountdummy, option];
            
            Return[dimlesscoef];
        ];        
        
        If[Length[argumentparts] == 1 && chosen2 == 0,
            chosen2 = 1;
            newargumentparts = {{{}, {}, {}}};
            scalarmults = {{{}, {}, {}}};
            
            (*Take out the scalars that arent functions of the appropriate independent variable.*)
            For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
                If[argumentparts[[1, 3, i]] == 0,
                    scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], \
                      {argumentparts[[1, 1, i]]}];
                    scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], \
                      {argumentparts[[1, 2, i]]}];
                    scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], \
                      {argumentparts[[1, 3, i]]}];
                    ,
                    newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                      {argumentparts[[1, 1, i]]}];
                    newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                      {argumentparts[[1, 2, i]]}];
                    newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                      {argumentparts[[1, 3, i]]}];
                ];
            ];
            
            (*Handle case of no scalars w.r.t. the average*)
            If[scalarmults == {{{}, {}, {}}},
                scalarmults = {{{1}, {0}, {0}}};
            ];
            
            (*Handle things in the average and multiply everything together*)
            If[newargumentparts == {{{}, {}, {}}},
                scalarmults2 = getterminfo[listProduct[scalarmults[[1, 1]]], Complement[ \
                  alldependencies, closuredependencies[[solformtermindexglobal]]], \
                  scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                  avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                scalarmults2 = MakeSingleTerm[scalarmults2, {}, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters];
                If[scalarmults2[[1, 3, 1]] == 1,
                    dimlesscoef = {{term, closureformwriter[closureprefix, \
                      closuredependencies, 0, {}, term]}};
                    ,
                    dimlesscoef = {{term, closureformwriter[closureprefix, \
                      closuredependencies, 0, {}, 1]}};
                ];
                dimlesscoef2 = caseDotSimp2[scalarmults, dimlesscoef, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                dimlesscoef[[1]] = Join[dimlesscoef[[1]], dimlesscoef2];
                
                ,
                
                newargumentpartsUTP = UnfoldMergedTerms[newargumentparts, dotdenoters, \
                  closuredependencies[[solformtermindexglobal]], scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
                
                dimlesscoef = {};
                For[i = 1, i <= Length[newargumentpartsUTP], i++,
                    dummy = caseDotSimp[{newargumentpartsUTP[[i]]}, solformterm, closureprefix, \
                      closuredependencies, alldependencies, scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
                    dummy[[1, 1]] = dummy[[1, 1]] * listProduct[scalarmults[[1, 1]]];
                    dummy[[1, 2, 1]] = dummy[[1, 2, 1]] * listProduct[scalarmults[[1, 1]]];
                    
                    dimlesscoef = Join[dimlesscoef, dummy];
                ];
                
                ,
                
                Print["CRITICAL ERROR: caseDot: Unable to identify newargumentparts. Term is: ", term];
                Return[Null];
            ];
            
            If[chosen2 == 0,
                Print["CRITICAL ERROR: caseDot: Unable to identify the number of terms coming back. Term is: ", term];
                Return[Null];
            ];
        ];
        
        Return[dimlesscoef];
    ];


caseDotSimp[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = term, chosen = 0, chosen2 = 0, i, argumentparts, replacevar, \
    inavg = 0, outavg = 0, success = 1, dependentIndices = {}, chosen3 = 0, newavgargument, \
    dummy, irank, newavgargumentRank},
        
        chosen = 0;
        
        (*If it is only one dotted term.*)
        If[Length[term[[1, 1]]] == 1,
            If[Head[term[[1, 1, 1]]] == List,
                
                inavg = 0;
                outavg = 0;
                success = 1;
                dependentIndices = {};
                For[i = 1, i <= Length[term[[1, 3, 1]]], i++,
                    If[term[[1, 3, 1, i]] == 1 && inavg == 1 && outavg == 0,
                        dependentIndices = Join[dependentIndices, {i}];
                    ];
                    If[term[[1, 3, 1, i]] == 1 && inavg == 0,
                        If[outavg == 0,
                            inavg = 1;
                            dependentIndices = Join[dependentIndices, {i}];
                            ,
                            success = 0;
                            Break[];
                        ];
                    ];
                    If[term[[1, 3, 1, i]] == 0 && inavg == 1,
                        inavg = 0;
                        outavg = 1;
                    ];
                ];
                
                
                If[success == 1,
                    chosen3 = 0;
                    
                    (*If only one dotted component is dependent on closure dependencies*)
                    If[Length[dependentIndices] == 1,
                        chosen = 1;
                        chosen3 = 1;
                        chosen2 = 0;
                        
                        argumentparts = getterminfo[term[[1, 1, 1, dependentIndices[[1]]]], \
                          Complement[alldependencies, closuredependencies[[ \
                          solformtermindexglobal]]], scalars, vectors, tensors, closurevar, \
                          graddenoters, divdenoters, avgdenoters, derivativedenoters, \
                          dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                          gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, \
                          option];
                        argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
                          tensorproductdenoters, doubledotdenoters];
                        
                        If[argumentparts[[1, 3, 1]] == 0,
                            chosen2 = 1;
                            replacevar = closureformwriter[closureprefix, \
                              closuredependencies, dimlesscoef[[1, 2, 1, \
                              dependentIndices[[1]]]], {}, 1];
                            If[dependentIndices[[1]] == 2 && dimlesscoef[[1, 2, 1, 1]] == 1 && \
                              dimlesscoef[[1, 2, 1, 2]] == 1,
                                dimlesscoef[[1, 1, 1, 2]] = dimlesscoef[[1, 1, 1, 1]];
                                dimlesscoef[[1, 3, 1, 2]] = dimlesscoef[[1, 3, 1, 1]];
                                dimlesscoef[[1, 1, 1, 1]] = replacevar[[2]];
                                dimlesscoef[[1, 3, 1, 1]] = 1;
                                ,
                                dimlesscoef[[1, 1, 1, dependentIndices[[1]]]] = replacevar[[2]];
                            ];
                            dimlesscoef = MergeUnfoldedTerms[dimlesscoef, dotdenoters, \
                              dotdenoters, tensorproductdenoters, doubledotdenoters];
                            dimlesscoef = {{listProduct[MergeUnfoldedTerms[term, \
                              dotdenoters, dotdenoters, tensorproductdenoters, \
                              doubledotdenoters][[1, 1]]], {dimlesscoef[[1, 1, 1]], \
                              replacevar[[2]], replacevar[[3]]}}};  
                        ];
                        
                        If[chosen2 == 0 && argumentparts[[1, 3, 1]] == 1,
                            Print["CRITICAL ERROR: caseDotSimp: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                            Return[Null];
                        ];
                    ];
                    
                    If[chosen3 == 0,
                        chosen = 1;
                        chosen3 = 1;
                        chosen2 = 0;
                        
                        (*Create term from the consectively dotted terms.*)
                        For[i = 1, i <= Length[dependentIndices], i++,
                            If[i == 1,
                                newavgargument = term[[1, 1, 1, dependentIndices[[i]]]];
                                ,
                                newavgargument = OperatorApplier[newavgargument, dotdenoters, \
                                  term[[1, 1, 1, dependentIndices[[i]]]]];
                            ];
                        ];
                        
                        (*Find the rank of the new term.*)
                        For[irank = 1, irank <= Length[dependentIndices], irank++,
                            If[irank == 1,
                                newavgargumentRank = term[[1, 2, 1, \
                                  dependentIndices[[irank]]]];
                                ,
                                newavgargumentRank = Abs[newavgargumentRank - term[[1, 2, 1, \
                                  dependentIndices[[irank]]]]];
                            ];
                        ];
                        
                        argumentparts = getterminfo[newavgargument, Complement[ \
                          alldependencies, closuredependencies[[solformtermindexglobal]]], \
                          scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                          avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                          doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                          fastIV, levelcountdummy, option];
                        argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
                          tensorproductdenoters, doubledotdenoters];
                        
                        If[argumentparts[[1, 3, 1]] == 0,
                            chosen2 = 1;
                            replacevar = closureformwriter[closureprefix, \
                              closuredependencies, newavgargumentRank, {}, 1];
                            dimlesscoef[[1, 1, 1, dependentIndices[[1]]]] = replacevar[[2]];
                            
                            (*Drop the components that were used to make the term.*)
                            If[Length[dependentIndices] > 1,
                                For[i = 2, i <= Length[dependentIndices], i++,
                                    dimlesscoef[[1, 1, 1]] = Drop[ \
                                      dimlesscoef[[1, 1, 1]], {dependentIndices[[i]]}];
                                    dimlesscoef[[1, 2, 1]] = Drop[ \
                                      dimlesscoef[[1, 2, 1]], {dependentIndices[[i]]}];
                                    dimlesscoef[[1, 3, 1]] = Drop[ \
                                      dimlesscoef[[1, 3, 1]], {dependentIndices[[i]]}];
                                ];
                            ];
                            
                            dimlesscoef = MergeUnfoldedTerms[dimlesscoef, dotdenoters, \
                              dotdenoters, tensorproductdenoters, doubledotdenoters];
                            dimlesscoef = {{listProduct[MergeUnfoldedTerms[term, \
                              dotdenoters, dotdenoters, tensorproductdenoters, \
                              doubledotdenoters][[1, 1]]], {dimlesscoef[[1, 1, 1]], \
                              replacevar[[2]], replacevar[[3]]}}};
                        ];
                        
                        If[chosen2 == 0 && argumentparts[[1, 3, 1]] == 1,
                            Print["CRITICAL ERROR: caseDotSimp: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                            Return[Null];
                        ];
                    ];
                    
                ];
                
            ];
        ];
        
        
        If[chosen == 0,
            dimlesscoef = MakeSingleTerm[dimlesscoef, dotdenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            argumentparts = getterminfo[dimlesscoef[[1, 1, 1]], Complement[alldependencies, \
              closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            If[argumentparts[[1, 3, 1]] == 0,
                dimlesscoef = {{argumentparts[[1, 1, 1]], closureformwriter[closureprefix, \
                  closuredependencies, argumentparts[[1, 2, 1]], {}, 1]}};
                ,
                Print["CRITICAL ERROR: caseDotSimp: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                Return[Null];
            ];
        ];
        
        Return[dimlesscoef];
    ];


caseDotSimp2[term_, dimlesscoefprev_, solformterm_, closureprefix_, closuredependencies_, \
alldependencies_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, \
avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, \
Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{i, ii, iii, argumentparts, dimlesscoef = {}, newargumentparts1, newargumentparts2, \
    scalarmults = {{{}, {}, {}}}, newargumentparts = {{{}, {}, {}}}, \
    currentclosvar = dimlesscoefprev[[1, 2, 2]]},
        
        argumentparts = getterminfo[listProduct[term[[1, 1]]], Complement[alldependencies, \
          closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
          levelcountdummy, option];
        
        If[Length[argumentparts] > 1,
            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
        ];
        
        newargumentparts = {{{}, {}, {}}};
        scalarmults = {{{}, {}, {}}};
        For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
            If[argumentparts[[1, 3, i]] == 0,
                scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], \
                  {argumentparts[[1, 1, i]]}];
                scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], \
                  {argumentparts[[1, 2, i]]}];
                scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], \
                  {argumentparts[[1, 3, i]]}];
                ,
                newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                  {argumentparts[[1, 1, i]]}];
                newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                  {argumentparts[[1, 2, i]]}];
                newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                  {argumentparts[[1, 3, i]]}];
            ];
        ];
        
        (*Handle case of no scalars w.r.t. the average*)
        If[scalarmults == {{{}, {}, {}}},
            scalarmults = {{{1}, {0}, {0}}};
        ];
        If[newargumentparts == {{{}, {}, {}}},
            newargumentparts = {{{1}, {0}, {0}}};
        ];
        
        
        (*Test for potential closure forms that are dot products of constants*)
        newargumentparts1 = UnfoldMergedTerms[newargumentparts, dotdenoters, Complement[ \
          alldependencies, closuredependencies[[solformtermindexglobal]]], scalars, vectors, \
          tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
          slowIV, fastIV, levelcountdummy, option];
        
        If[Length[newargumentparts1] > 1,
            Print["Warning: caseDotSimp2: Could not run caseDotSimp2, as term came back as a sum. Term is: ", term];
            Return[{}];
        ];
        
        For[ii = 1, ii <= Length[newargumentparts1[[1, 1]]], ii++,
            If[Head[newargumentparts1[[1, 1, ii]]] == List,
                For[iii = 1, iii <= Length[newargumentparts1[[1, 3, ii]]], iii++,
                    If[newargumentparts1[[1, 3, ii, iii]] == 0,
                        newargumentparts2 = newargumentparts1;
                        newargumentparts2[[1, 1, ii, iii]] = currentclosvar;
                        newargumentparts2 = MakeSingleTerm[newargumentparts2, dotdenoters, \
                          dotdenoters, tensorproductdenoters, doubledotdenoters];
                        dimlesscoef = Join[dimlesscoef, {{newargumentparts2[[1, 1, 1]], \
                          currentclosvar, newargumentparts1[[1, 2, ii, iii]]}}];
                    ];
                ];
            ];
        ];
        
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseTensorProduct[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, \
vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef, Nterms = Length[term], i, ii, j, iii, chosen2, distributedtensor, \
    expandedterm, argumentparts, newargumentparts = {{{}, {}, {}}}, \
    scalarmults = {{{}, {}, {}}}, newargumentpartsUTP, dummy, scalarmults2},
        
        For[i = 1, i <= Nterms, i++,
            If[Head[term[[i]]] == Plus,
                expandedterm = 0;
                For[ii = 1, ii <= Length[term[[i]]], ii++,
                    For[iii = 1, iii <= Nterms, iii++,
                        If[iii == 1,
                            If[iii == i,
                                distributedtensor = term[[iii, ii]];
                                ,
                                distributedtensor = term[[iii]];
                            ];
                            ,
                            If[iii == i,
                                distributedtensor = OperatorApplier[distributedtensor, \
                                  tensorproductdenoters, term[[iii, ii]]];
                                ,
                                distributedtensor = OperatorApplier[distributedtensor, \
                                  tensorproductdenoters, term[[iii]]];
                            ];
                        ];
                    ];
                    expandedterm += distributedtensor;
                ];
                
                dimlesscoef = decider[expandedterm, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                Return[dimlesscoef];
            ];
        ];
        
        
        
        
        
        argumentparts = getterminfo[term, closuredependencies[[solformtermindexglobal]], \
          scalars, vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
          dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
          slowIV, fastIV, levelcountdummy, option];
        
        chosen2 = 0;
        If[Length[argumentparts] > 1,
            chosen2 = 1;
            newargumentparts = 0;
            For[i = 1, i <= Length[argumentparts], i++,
                newargumentparts += listProduct[argumentparts[[i, 1]]];
            ];
            newargumentparts = Expand[newargumentparts];
            
            dimlesscoef = decider[newargumentparts, solformterm, closureprefix, closuredependencies, \
              alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            
            Return[dimlesscoef];
        ];
        
        If[Length[argumentparts] == 1 && chosen2 == 0,
            chosen2 = 1;
            newargumentparts = {{{}, {}, {}}};
            scalarmults = {{{}, {}, {}}};
            
            (*Take out the scalars that arent functions of the appropriate independent variable.*)
            For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
                If[argumentparts[[1, 3, i]] == 0,
                    scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], \
                      {argumentparts[[1, 1, i]]}];
                    scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], \
                      {argumentparts[[1, 2, i]]}];
                    scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], \
                      {argumentparts[[1, 3, i]]}];
                    ,
                    newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                      {argumentparts[[1, 1, i]]}];
                    newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                      {argumentparts[[1, 2, i]]}];
                    newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                      {argumentparts[[1, 3, i]]}];
                ];
            ];
            
            (*Handle case of no scalars w.r.t. the average*)
            If[scalarmults == {{{}, {}, {}}},
                scalarmults = {{{1}, {0}, {0}}};
            ];
            
            (*Handle things in the average and multiply everything together*)
            If[newargumentparts == {{{}, {}, {}}},
                scalarmults2 = getterminfo[listProduct[scalarmults[[1, 1]]], Complement[ \
                  alldependencies, closuredependencies[[solformtermindexglobal]]], \
                  scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                  avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                scalarmults2 = MakeSingleTerm[scalarmults2, {}, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters];
                If[scalarmults2[[1, 3, 1]] == 1,
                    dimlesscoef = {{term, closureformwriter[closureprefix, \
                      closuredependencies, 0, {}, term]}};
                    ,
                    dimlesscoef = {{term, closureformwriter[closureprefix, \
                      closuredependencies, 0, {}, 1]}};
                ];
                
                ,
                
                newargumentpartsUTP = UnfoldMergedTerms[newargumentparts, \
                  tensorproductdenoters, closuredependencies[[solformtermindexglobal]], \
                  scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                  avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                dimlesscoef = {};
                For[i = 1, i <= Length[newargumentpartsUTP], i++,
                    dummy = caseTensorProductSimp[{newargumentpartsUTP[[i]]}, solformterm, closureprefix, \
                      closuredependencies, alldependencies, scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
                    dummy[[1, 1]] = dummy[[1, 1]] * listProduct[scalarmults[[1, 1]]];
                    dummy[[1, 2, 1]] = dummy[[1, 2, 1]] * listProduct[scalarmults[[1, 1]]];
                    
                    dimlesscoef = Join[dimlesscoef, dummy];
                ];
                
                ,
                
                Print["CRITICAL ERROR: caseTensorProduct: Unable to identify newargumentparts. Term is: ", term];
                Return[Null];
            ];
            
            If[chosen2 == 0,
                Print["CRITICAL ERROR: caseTensorProduct: Unable to identify the number of terms coming back. Term is: ", term];
                Return[Null];
            ];
        ];
        
        Return[dimlesscoef];
    ];


caseTensorProductSimp[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, \
vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = term, chosen = 0, chosen2 = 0, i, argumentparts, replacevar},
        
        chosen = 0;
        (*If only one tensor producted term is dependent on closure dependencies*)
        If[Length[term[[1, 1]]] == 1,
            If[Head[term[[1, 1, 1]]] == List,
                If[Total[term[[1, 3, 1]]] == 1,
                    chosen = 1;
                    For[i = 1, i <= Length[term[[1, 3, 1]]], i++,
                        If[term[[1, 3, 1, i]] == 1,
                            
                            chosen2 = 0;
                            
                            argumentparts = getterminfo[term[[1, 1, 1, i]], Complement[ \
                              alldependencies, closuredependencies[[solformtermindexglobal]]], \
                              scalars, vectors, tensors, closurevar, graddenoters, \
                              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, \
                              divform, gradform, normalvecform, Uform, slowIV, fastIV, \
                              levelcountdummy, option];
                              
                            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
                              tensorproductdenoters, doubledotdenoters];
                            
                            If[argumentparts[[1, 3, 1]] == 0,
                                chosen2 = 1;
                                replacevar = closureformwriter[closureprefix, \
                                  closuredependencies, dimlesscoef[[1, 2, 1, i]], {}, 1];
                                dimlesscoef[[1, 1, 1, i]] = replacevar[[2]];
                                dimlesscoef = MergeUnfoldedTerms[dimlesscoef, \
                                  tensorproductdenoters, dotdenoters, tensorproductdenoters, \
                                  doubledotdenoters];
                                dimlesscoef = {{listProduct[MergeUnfoldedTerms[term, \
                                  tensorproductdenoters, dotdenoters, tensorproductdenoters, \
                                  doubledotdenoters][[1, 1]]], {dimlesscoef[[1, 1, 1]], \
                                  replacevar[[2]], replacevar[[3]]}}};  
                            ];
                            
                            If[chosen2 == 0 && argumentparts[[1, 3, 1]] == 1,
                                Print["CRITICAL ERROR: caseTensorProductSimp: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                                Return[Null];
                            ];
                            
                            Break[];
                        ];
                    ];
                ];
            ];
        ];
        
        
        If[chosen == 0,
            dimlesscoef = MakeSingleTerm[dimlesscoef, tensorproductdenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            argumentparts = getterminfo[dimlesscoef[[1, 1, 1]], Complement[alldependencies, \
              closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            If[argumentparts[[1, 3, 1]] == 0,
                dimlesscoef = {{argumentparts[[1, 1, 1]], closureformwriter[closureprefix, \
                  closuredependencies, argumentparts[[1, 2, 1]], {}, 1]}};
                ,
                Print["CRITICAL ERROR: caseTensorProductSimp: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                Return[Null];
            ];
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseDoubleDot[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, \
vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, Nterms = Length[term], i, ii, j, iii, chosen2, distributeddoubledot, \
    expandedterm, argumentparts, newargumentparts = {{{}, {}, {}}}, scalarmults = {{{}, {}, {}}}, \
    newargumentpartsUTP, dummy, scalarmults2},
        
        For[i = 1, i <= Nterms, i++,
            If[Head[term[[i]]] == Plus,
                expandedterm = 0;
                For[ii = 1, ii <= Length[term[[i]]], ii++,
                    For[iii = 1, iii <= Nterms, iii++,
                        If[iii == 1,
                            If[iii == i,
                                distributeddoubledot = term[[iii, ii]];
                                ,
                                distributeddoubledot = term[[iii]];
                            ];
                            ,
                            If[iii == i,
                                distributeddoubledot = OperatorApplier[distributeddoubledot, \
                                  doubledotdenoters, term[[iii, ii]]];
                                ,
                                distributeddoubledot = OperatorApplier[distributeddoubledot, \
                                  doubledotdenoters, term[[iii]]];
                            ];
                        ];
                    ];
                    expandedterm += distributeddoubledot;
                ];
                
                dimlesscoef = decider[expandedterm, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                Return[dimlesscoef];
            ];
        ];
        
        
        
        
        
        argumentparts = getterminfo[term, closuredependencies[[solformtermindexglobal]], \
          scalars, vectors, tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, \
          dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
          slowIV, fastIV, levelcountdummy, option];
        
        chosen2 = 0;
        If[Length[argumentparts] > 1,
            chosen2 = 1;
            newargumentparts = 0;
            For[i = 1, i <= Length[argumentparts], i++,
                newargumentparts += listProduct[argumentparts[[i, 1]]];
            ];
            newargumentparts = Expand[newargumentparts];
            
            dimlesscoef = decider[newargumentparts, solformterm, closureprefix, closuredependencies, \
              alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
              normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            
            Return[dimlesscoef];
        ];
        
        If[Length[argumentparts] == 1,
            chosen2 = 1;
            newargumentparts = {{{}, {}, {}}};
            scalarmults = {{{}, {}, {}}};
            
            (*Take out the scalars that arent functions of the appropriate independent variable.*)
            For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
                If[argumentparts[[1, 3, i]] == 0,
                    scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], \
                      {argumentparts[[1, 1, i]]}];
                    scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], \
                      {argumentparts[[1, 2, i]]}];
                    scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], \
                      {argumentparts[[1, 3, i]]}];
                    ,
                    newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                      {argumentparts[[1, 1, i]]}];
                    newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                      {argumentparts[[1, 2, i]]}];
                    newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                      {argumentparts[[1, 3, i]]}];
                ];
            ];
            
            (*Handle case of no scalars w.r.t. the average*)
            If[scalarmults == {{{}, {}, {}}},
                scalarmults = {{{1}, {0}, {0}}};
            ];
            
            (*Handle things in the average and multiply everything together*)
            If[newargumentparts == {{{}, {}, {}}},
                scalarmults2 = getterminfo[listProduct[scalarmults[[1, 1]]], Complement[ \
                  alldependencies, closuredependencies[[solformtermindexglobal]]], \
                  scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                  avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                scalarmults2 = MakeSingleTerm[scalarmults2, {}, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters];
                If[scalarmults2[[1, 3, 1]] == 1,
                    dimlesscoef = {{term, closureformwriter[closureprefix, \
                      closuredependencies, 0, {}, term]}};
                    ,
                    dimlesscoef = {{term, closureformwriter[closureprefix, \
                      closuredependencies, 0, {}, 1]}};
                ];
                
                ,
                
                newargumentpartsUTP = UnfoldMergedTerms[newargumentparts, doubledotdenoters, \
                  closuredependencies[[solformtermindexglobal]], scalars, vectors, tensors, \
                  closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                  tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
                  Uform, slowIV, fastIV, levelcountdummy, option];
                
                dimlesscoef = {};
                For[i = 1, i <= Length[newargumentpartsUTP], i++,
                    dummy = caseDoubleDotSimp[{newargumentpartsUTP[[i]]}, solformterm, closureprefix, \
                      closuredependencies, alldependencies, scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    
                    dummy[[1, 1]] = dummy[[1, 1]] * listProduct[scalarmults[[1, 1]]];
                    dummy[[1, 2, 1]] = dummy[[1, 2, 1]] * listProduct[scalarmults[[1, 1]]];
                    
                    dimlesscoef = Join[dimlesscoef, dummy];
                ];
                
                ,
                
                Print["CRITICAL ERROR: caseDoubleDot: Unable to identify newargumentparts. Term is: ", term];
                Return[Null];
            ];
            
            If[chosen2 == 0,
                Print["CRITICAL ERROR: caseDoubleDot: Unable to identify the number of terms coming back. Term is: ", term];
                Return[Null];
            ];
        ];
        
        Return[dimlesscoef];
    ];


caseDoubleDotSimp[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, \
vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, \
slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef = term, chosen = 0, chosen2 = 0, i, argumentparts, replacevar},
        
        chosen = 0;
        (*If only one double dotted term is dependent on closure dependencies*)
        If[Length[term[[1, 1]]] == 1,
            If[Head[term[[1, 1, 1]]] == List,
                If[Total[term[[1, 3, 1]]] == 1,
                    chosen = 1;
                    For[i = 1, i <= Length[term[[1, 3, 1]]], i++,
                        If[term[[1, 3, 1, i]] == 1,
                            
                            chosen2 = 0;
                            
                            argumentparts = getterminfo[term[[1, 1, 1, i]], Complement[ \
                              alldependencies, closuredependencies[[solformtermindexglobal]]], \
                              scalars, vectors, tensors, closurevar, graddenoters, \
                              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, \
                              doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
                              slowIV, fastIV, levelcountdummy, option];
                              
                            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
                              tensorproductdenoters, doubledotdenoters];
                            
                            If[argumentparts[[1, 3, 1]] == 0,
                                chosen2 = 1;
                                replacevar = closureformwriter[closureprefix, \
                                  closuredependencies, dimlesscoef[[1, 2, 1, i]], {}, 1];
                                dimlesscoef[[1, 1, 1, i]] = replacevar[[2]];
                                dimlesscoef = MergeUnfoldedTerms[dimlesscoef, \
                                  doubledotdenoters, dotdenoters, tensorproductdenoters, \
                                  doubledotdenoters];
                                dimlesscoef = {{listProduct[MergeUnfoldedTerms[term, \
                                  doubledotdenoters, dotdenoters, tensorproductdenoters, \
                                  doubledotdenoters][[1, 1]]], {dimlesscoef[[1, 1, 1]], \
                                  replacevar[[2]], replacevar[[3]]}}};  
                            ];
                            
                            If[chosen2 == 0 && argumentparts[[1, 3, 1]] == 1,
                                Print["CRITICAL ERROR: caseDoubleDotSimp: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                                Return[Null];
                            ];
                            
                            Break[];
                        ];
                    ];
                ];
            ];
        ];
        
        
        If[chosen == 0,
            dimlesscoef = MakeSingleTerm[dimlesscoef, doubledotdenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            argumentparts = getterminfo[dimlesscoef[[1, 1, 1]], Complement[alldependencies, \
              closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, \
              Uform, slowIV, fastIV, levelcountdummy, option];
            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            If[argumentparts[[1, 3, 1]] == 0,
                dimlesscoef = {{argumentparts[[1, 1, 1]], closureformwriter[closureprefix, \
                  closuredependencies, argumentparts[[1, 2, 1]], {}, 1]}};
                ,
                Print["CRITICAL ERROR: caseDoubleDotSimp: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                Return[Null];
            ];
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
casePlus[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, i, dummy},
        
        For[i = 1, i <= Length[term], i++,
            dummy = decider[term[[i]], solformterm, closureprefix, closuredependencies, \
              alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
              divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
              gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
            dimlesscoef = Join[dimlesscoef, dummy];
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseGradient[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, subscript = term[[2]], argument = term[[1]], argumentparts, i, \
    expandedargument, dimlesscoefparts, bracketeddummy, chosen2 = 0, \
    newargumentparts = {{{}, {}, {}}}, flaggradientsubscript = 0, scalarmults2, \
    scalarmults = {{{}, {}, {}}}, checkdependencyandscalar, newargumentparts2},
        
        expandedargument = Expand[argument];
        
        If[Head[expandedargument] == Plus,
            dimlesscoefparts = {};
            dimlesscoef = {};
            For[i = 1, i <= Length[expandedargument], i++,
                checkdependencyandscalar = getterminfo[expandedargument[[i]], subscript, \
                  scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                  avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                  fastIV, levelcountdummy, option];
                  
                checkdependencyandscalar = MakeSingleTerm[checkdependencyandscalar, {}, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                
                If[checkdependencyandscalar[[1, 3, 1]] == 1,
                    bracketeddummy = OperatorApplier[expandedargument[[i]], graddenoters, \
                      subscript];
                    dimlesscoefparts = decider[bracketeddummy, solformterm, closureprefix, \
                      closuredependencies, alldependencies, scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    dimlesscoef = Join[dimlesscoef, dimlesscoefparts];
                ];
            ];
            
            Return[dimlesscoef];
        ];
        
        
        
        
        
        checkdependencyandscalar = getterminfo[argument, subscript, scalars, vectors, \
          tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
          levelcountdummy, option];
        
        checkdependencyandscalar = MakeSingleTerm[checkdependencyandscalar, {}, dotdenoters, \
          tensorproductdenoters, doubledotdenoters];
        
        If[checkdependencyandscalar[[1, 3, 1]] == 0,
            dimlesscoef = {{0, {0, 0, 0}}};
            Return[dimlesscoef];
        ];
        
        
        
        flaggradientsubscript = 0;
        If[Intersection[closuredependencies[[solformtermindexglobal]], subscript] != {},
            flaggradientsubscript = 1;
            
            argumentparts = getterminfo[argument, Complement[alldependencies, \
              closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
            
            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            
            If[argumentparts[[1, 3, 1]] == 0,
                dimlesscoef = {{term, closureformwriter[closureprefix, closuredependencies, \
                  argumentparts[[1, 2, 1]] + 1, {}, 1]}};
                ,
                Print["CRITICAL ERROR: caseGradient: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                Return[Null];
            ];
        ];
        
        If[flaggradientsubscript == 0,
            
            argumentparts = getterminfo[argument, \
              closuredependencies[[solformtermindexglobal]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
            
            chosen2 = 0;
            If[Length[argumentparts] > 1,
                chosen2 = 1;
                newargumentparts = 0;
                For[i = 1, i <= Length[argumentparts], i++,
                    newargumentparts += OperatorApplier[listProduct[argumentparts[[i, 1]]], \
                      graddenoters, subscript];
                ];
                newargumentparts = Expand[newargumentparts];
                
                dimlesscoef = decider[newargumentparts, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                  normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                Return[dimlesscoef];
            ];
            
            If[Length[argumentparts] == 1,
                chosen2 = 1;
                newargumentparts = {{{}, {}, {}}};
                scalarmults = {{{}, {}, {}}};
                
                For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
                    If[argumentparts[[1, 3, i]] == 0,
                        scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], \
                          {argumentparts[[1, 1, i]]}];
                        scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], \
                          {argumentparts[[1, 2, i]]}];
                        scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], \
                          {argumentparts[[1, 3, i]]}];
                        ,
                        newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                          {argumentparts[[1, 1, i]]}];
                        newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                          {argumentparts[[1, 2, i]]}];
                        newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                          {argumentparts[[1, 3, i]]}];
                    ];
                ];
                
                (*Handle case of no scalars w.r.t. the average*)
                If[scalarmults == {{{}, {}, {}}},
                    scalarmults = {{{1}, {0}, {0}}};
                ];
                
                If[newargumentparts == {{{}, {}, {}}},
                    scalarmults2 = getterminfo[listProduct[scalarmults[[1, 1]]], Complement[ \
                      alldependencies, closuredependencies[[solformtermindexglobal]]], \
                      scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                      avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                      normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                    scalarmults2 = MakeSingleTerm[scalarmults2, {}, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters];
                    dimlesscoef = OperatorApplier[listProduct[scalarmults[[1, 1]]], \
                      graddenoters, subscript];
                    If[scalarmults2[[1, 3, 1]] == 1,
                        dimlesscoef = {{dimlesscoef, closureformwriter[closureprefix, \
                          closuredependencies, 0, {}, dimlesscoef]}};
                        ,
                        dimlesscoef = {{dimlesscoef, closureformwriter[closureprefix, \
                          closuredependencies, 0, {}, 1]}};
                    ];
                    
                    ,
                    
                    newargumentparts2 = getterminfo[listProduct[newargumentparts[[1, 1]]], Complement[alldependencies, \
                      closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    newargumentparts2 = MakeSingleTerm[newargumentparts2, {}, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters];
                    
                    If[newargumentparts2[[1, 3, 1]] == 1,
                        Print["CRITICAL ERROR: caseGradient: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                        Return[Null];
                        ,
                        Print["CRITICAL ERROR: caseGradient: Really not expecting this option... term is: ", term];
                        Return[Null];
                    ];
                    
                    ,
                    
                    Print["CRITICAL ERROR: caseGradient: Unable to identify newargumentparts. Term is: ", term];
                    Return[Null];
                ];
            ];
            
            If[chosen2 == 0,
                Print["CRITICAL ERROR: caseGradient: Unable to identify the number of terms coming back. Term is: ", term];
                Return[Null];
            ];
            
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseDivergence[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, \
vectors_, tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, subscript = term[[2]], argument = term[[1]], argumentparts, i, \
    expandedargument, dimlesscoefparts, bracketeddummy, chosen2 = 0, \
    newargumentparts = {{{}, {}, {}}}, flaggradientsubscript = 0, dummy, \
    scalarmults = {{{}, {}, {}}}, checkdependencyandscalar, newargumentparts2},
        
        expandedargument = Expand[argument];
        
        If[Head[expandedargument] == Plus,
            dimlesscoefparts = {};
            dimlesscoef = 0;
            For[i = 1, i <= Length[expandedargument], i++,
                checkdependencyandscalar = getterminfo[expandedargument[[i]], subscript, \
                  scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                  avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                  fastIV, levelcountdummy, option];
                  
                checkdependencyandscalar = MakeSingleTerm[checkdependencyandscalar, {}, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                
                If[checkdependencyandscalar[[1, 2, 1]] > 0,
                    If[checkdependencyandscalar[[1, 3, 1]] == 1,
                        bracketeddummy = OperatorApplier[expandedargument[[i]], divdenoters, \
                          subscript];
                        dimlesscoefparts = decider[bracketeddummy, solformterm, closureprefix, \
                          closuredependencies, alldependencies, scalars, vectors, tensors, \
                          closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
                          slowIV, fastIV, levelcountdummy, option];
                        dimlesscoef = Join[dimlesscoef, dimlesscoefparts];
                    ];
                    ,
                    Print["CRITICAL ERROR: caseDivergence: Divergence was taken on a scalar. Term is: ", term];
                    Return[Null];
                ];
                
            ];
            
            Return[dimlesscoef];
        ];
        
        
        
        
        
        (*Check to see if Divergence is appropriate, or if you can return 0*)
        checkdependencyandscalar = getterminfo[argument, subscript, scalars, vectors, \
          tensors, closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
          tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, \
          slowIV, fastIV, levelcountdummy, option];
          
        checkdependencyandscalar = MakeSingleTerm[checkdependencyandscalar, {}, dotdenoters, \
          tensorproductdenoters, doubledotdenoters];
        
        If[checkdependencyandscalar[[1, 2, 1]] > 0,
            If[checkdependencyandscalar[[1, 3, 1]] == 0,
                dimlesscoef = {{0, {0, 0, 0}}};
                Return[dimlesscoef];
            ];
            ,
            Print["CRITICAL ERROR: caseDivergence: Divergence was taken on a scalar. Term is: ", term];
            Return[Null];
        ];
        
               
        
        flaggradientsubscript = 0;
        If[Intersection[closuredependencies[[solformtermindexglobal]], subscript] != {},
            flaggradientsubscript = 1;
            
            argumentparts = getterminfo[argument, Complement[alldependencies, \
              closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
            
            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            
            If[argumentparts[[1, 2, 1]] > 0,
                If[argumentparts[[1, 3, 1]] == 0,
                    dimlesscoef = {{term, closureformwriter[closureprefix, closuredependencies, \
                      argumentparts[[1, 2, 1]] - 1, {}, 1]}};
                    ,
                    Print["CRITICAL ERROR: caseDivergence: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                    Return[Null];
                ];
                ,
                Print["CRITICAL ERROR: caseDivergence: Divergence was taken on a scalar. Term is: ", term];
                Return[Null];
            ];
        ];
        
        If[flaggradientsubscript == 0,
            
            argumentparts = getterminfo[argument, \
              closuredependencies[[solformtermindexglobal]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
            
            chosen2 = 0;
            If[Length[argumentparts] > 1,
                chosen2 = 1;
                newargumentparts = 0;
                For[i = 1, i <= Length[argumentparts], i++,
                    dummy = MakeSingleTerm[{argumentparts[[i]]}, {}, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters];
                    If[dummy[[1, 2, 1]] > 0,
                        newargumentparts += OperatorApplier[listProduct[ \
                          argumentparts[[i, 1]]], divdenoters, subscript];
                        ,
                        Print["CRITICAL ERROR: caseDivergence: Divergence was taken on a scalar. Term is: ", term];
                        Return[Null];
                    ];
                ];
                newargumentparts = Expand[newargumentparts];
                
                dimlesscoef = decider[newargumentparts, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                Return[dimlesscoef];
            ];
            
            If[Length[argumentparts] == 1,
                chosen2 = 1;
                newargumentparts = {{{}, {}, {}}};
                scalarmults = {{{}, {}, {}}};
                
                For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
                    If[argumentparts[[1, 3, i]] == 0 && argumentparts[[1, 2, i]] == 0,
                        scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], \
                          {argumentparts[[1, 1, i]]}];
                        scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], \
                          {argumentparts[[1, 2, i]]}];
                        scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], \
                          {argumentparts[[1, 3, i]]}];
                        ,
                        newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                          {argumentparts[[1, 1, i]]}];
                        newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                          {argumentparts[[1, 2, i]]}];
                        newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                          {argumentparts[[1, 3, i]]}];
                    ];
                ];
                
                (*Handle case of no scalars w.r.t. the average*)
                If[scalarmults == {{{}, {}, {}}},
                    scalarmults = {{{1}, {0}, {0}}};
                ];
                
                (*Handle things in the average and multiply everything together*)
                If[newargumentparts == {{{}, {}, {}}},
                    
                    Print["CRITICAL ERROR: caseDivergence: I dont think it should get here... term is: ", term];
                    Return[Null];
                    
                    ,
                    
                    newargumentparts2 = MakeSingleTerm[newargumentparts, {}, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters];
                    newargumentparts2 = getterminfo[newargumentparts2, Complement[alldependencies, \
                      closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    newargumentparts2 = MakeSingleTerm[newargumentparts2, {}, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters];
                    
                    If[newargumentparts2[[1, 3, 1]] == 1,
                        Print["CRITICAL ERROR: caseDivergence: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                        Return[Null];
                        ,
                        Print["CRITICAL ERROR: caseDivergence: Really not expecting this option... term is: ", term];
                        Return[Null];
                    ];
                    
                    ,
                    
                    Print["CRITICAL ERROR: caseDivergence: Unable to identify newargumentparts. Term is: ", term];
                    Return[Null];
                ];
            ];
            
            If[chosen2 == 0,
                Print["CRITICAL ERROR: caseDivergence: Unable to identify the number of terms coming back. Term is: ", term];
                Return[Null];
            ];
            
        ];
        
        Return[dimlesscoef];
    ];


(*Ready*)
caseAverage[term_, solformterm_, closureprefix_, closuredependencies_, alldependencies_, scalars_, vectors_, \
tensors_, closurevar_, graddenoters_, divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, \
tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, \
levelcountdummy_, option_] :=
    Block[{dimlesscoef = {}, subscript = term[[2]], argument = term[[1, 1]], \
    argumentparts, i, expandedargument, dimlesscoefparts, bracketeddummy, \
    flagavgoverIV = 0, avgdenoterssymbols = avgdenoters[[2, ;;, 1]], \
    avgdenotersIV = avgdenoters[[2, ;;, 2]], avgoverIV, avgoversymbol, chosen2 = 0, \
    newargumentparts = {{{}, {}, {}}}, flaggradientsubscript = 0, \
    scalarmults = {{{}, {}, {}}}, checkdependencyandscalar, dummy, flagsubscriptIV = 0, \
    subscriptIV, newargumentpartsCAS, scalarmults2, \
    avgdenoterssplitdomainsymbols = avgdenoters[[2, ;;, 3]], newargumentparts2},
        
        expandedargument = Expand[argument];
        
        If[Head[expandedargument] == Plus,
            flagsubscriptIV = 0;
            For[i = 1, i <= Length[avgdenoterssymbols], i++,
                If[Intersection[{subscript}, avgdenoterssymbols[[i]]] != {} || \
                  Intersection[{subscript}, Flatten[avgdenoterssplitdomainsymbols[[i]]]] != {},
                    flagsubscriptIV = 1;
                    subscriptIV = avgdenotersIV[[i]];
                    Break[];
                ];
            ];
            If[flagsubscriptIV == 0,
                Print["CRITICAL ERROR: caseAverage: Unidentified average in variable subscript. Variable is: ", subscript];
                Return[Null];
            ];
            
            dimlesscoefparts = {};
            dimlesscoef = {};
            For[i = 1, i <= Length[expandedargument], i++,
                checkdependencyandscalar = getterminfo[expandedargument[[i]], subscriptIV, \
                  scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                  avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                  fastIV, levelcountdummy, option];
                
                checkdependencyandscalar = MakeSingleTerm[checkdependencyandscalar, {}, \
                  dotdenoters, tensorproductdenoters, doubledotdenoters];
                
                If[checkdependencyandscalar[[1, 3, 1]] == 1,
                    bracketeddummy = OperatorApplier[expandedargument[[i]], \
                      avgdenoters[[1]], subscript];
                    dimlesscoefparts = decider[bracketeddummy, solformterm, closureprefix, \
                      closuredependencies, alldependencies, scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    dimlesscoef = Join[dimlesscoef, dimlesscoefparts];
                    ,
                    dimlesscoefparts = decider[expandedargument[[i]], solformterm, closureprefix, \
                      closuredependencies, alldependencies, scalars, vectors, tensors, \
                      closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, \
                      fastIV, levelcountdummy, option];
                    dimlesscoef = Join[dimlesscoef, dimlesscoefparts];
                ];
            ];
            
            Return[dimlesscoef];
        ];
        
        
        
        
        
        (*Figure out the independent variables to the averaging operator*)
        flagsubscriptIV = 0;
        For[i = 1, i <= Length[avgdenoterssymbols], i++,
            If[Intersection[{subscript}, avgdenoterssymbols[[i]]] != {} || \
              Intersection[{subscript}, Flatten[avgdenoterssplitdomainsymbols[[i]]]] != {},
                flagsubscriptIV = 1;
                subscriptIV = avgdenotersIV[[i]];
                Break[];
            ];
        ];
        If[flagsubscriptIV == 0,
            Print["CRITICAL ERROR: caseAverage: Unidentified average in variable subscript. Variable is: ", subscript];
            Return[Null];
        ];
        
        
        
        flaggradientsubscript = 0;
        If[Intersection[closuredependencies[[solformtermindexglobal]], subscriptIV] != {},,
            flaggradientsubscript = 1;
            
            argumentparts = getterminfo[argument, Complement[alldependencies, \
              closuredependencies[[solformtermindexglobal]]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
            
            argumentparts = MakeSingleTerm[argumentparts, {}, dotdenoters, \
              tensorproductdenoters, doubledotdenoters];
            
            If[argumentparts[[1, 3, 1]] == 0,
                dimlesscoef = {{term, closureformwriter[closureprefix, closuredependencies, \
                  argumentparts[[1, 2, 1]], {}, 1]}};
                ,
                Print["CRITICAL ERROR: caseAverage: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                Return[Null];
            ];
        ];
        
        If[flaggradientsubscript == 0,
            
            argumentparts = getterminfo[argument, \
              closuredependencies[[solformtermindexglobal]], scalars, vectors, tensors, \
              closurevar, graddenoters, divdenoters, avgdenoters, derivativedenoters, dotdenoters, \
              tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, normalvecform, Uform, slowIV, fastIV, \
              levelcountdummy, option];
            
            chosen2 = 0;
            If[Length[argumentparts] > 1,
                chosen2 = 1;
                newargumentparts = 0;
                For[i = 1, i <= Length[argumentparts], i++,
                    newargumentparts += OperatorApplier[listProduct[argumentparts[[i, 1]]], \
                      avgdenoters[[1]], subscript];
                ];
                newargumentparts = Expand[newargumentparts];
                
                dimlesscoef = decider[newargumentparts, solformterm, closureprefix, closuredependencies, \
                  alldependencies, scalars, vectors, tensors, closurevar, graddenoters, \
                  divdenoters, avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, \
                  gradform, normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                
                Return[dimlesscoef];
            ];
            
            If[Length[argumentparts] == 1,
                chosen2 = 1;
                newargumentparts = {{{}, {}, {}}};
                scalarmults = {{{}, {}, {}}};
                
                (*Take out the scalars that arent functions of the appropriate independent
                variable.*)
                For[i = 1, i <= Length[argumentparts[[1, 1]]], i++,
                    If[argumentparts[[1, 3, i]] == 0,
                        scalarmults[[1, 1]] = Join[scalarmults[[1, 1]], \
                          {argumentparts[[1, 1, i]]}];
                        scalarmults[[1, 2]] = Join[scalarmults[[1, 2]], \
                          {argumentparts[[1, 2, i]]}];
                        scalarmults[[1, 3]] = Join[scalarmults[[1, 3]], \
                          {argumentparts[[1, 3, i]]}];
                        ,
                        newargumentparts[[1, 1]] = Join[newargumentparts[[1, 1]], \
                          {argumentparts[[1, 1, i]]}];
                        newargumentparts[[1, 2]] = Join[newargumentparts[[1, 2]], \
                          {argumentparts[[1, 2, i]]}];
                        newargumentparts[[1, 3]] = Join[newargumentparts[[1, 3]], \
                          {argumentparts[[1, 3, i]]}];
                    ];
                ];
                
                (*Handle case of no scalars w.r.t. the average*)
                If[scalarmults == {{{}, {}, {}}},
                    scalarmults = {{{1}, {0}, {0}}};
                ];
                
                (*Handle things in the average and multiply everything together*)
                If[newargumentparts == {{{}, {}, {}}},
                    scalarmults2 = getterminfo[listProduct[scalarmults[[1, 1]]], Complement[ \
                      alldependencies, closuredependencies[[solformtermindexglobal]]], \
                      scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                      avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                      normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                    scalarmults2 = MakeSingleTerm[scalarmults2, {}, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters];
                    dimlesscoef = OperatorApplier[listProduct[scalarmults[[1, 1]]], \
                      avgdenoters[[1]], subscript];
                    If[scalarmults2[[1, 3, 1]] == 1,
                        dimlesscoef = {{dimlesscoef, closureformwriter[closureprefix, \
                          closuredependencies, 0, {}, dimlesscoef]}};
                        ,
                        dimlesscoef = {{dimlesscoef, closureformwriter[closureprefix, \
                          closuredependencies, 0, {}, 1]}};
                    ];
                    
                    ,
                    
                    newargumentparts2 = MakeSingleTerm[newargumentparts, {}, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters];
                    newargumentparts2 = getterminfo[newargumentparts2, Complement[ \
                      alldependencies, closuredependencies[[solformtermindexglobal]]], \
                      scalars, vectors, tensors, closurevar, graddenoters, divdenoters, \
                      avgdenoters, derivativedenoters, dotdenoters, tensorproductdenoters, doubledotdenoters, pietrzykflipdenoters, divform, gradform, \
                      normalvecform, Uform, slowIV, fastIV, levelcountdummy, option];
                    newargumentparts2 = MakeSingleTerm[newargumentparts2, {}, dotdenoters, \
                      tensorproductdenoters, doubledotdenoters];
                    
                    If[newargumentparts2[[1, 3, 1]] == 1,
                        Print["CRITICAL ERROR: caseAverage: Term encountered that relies partially on the planned dependencies of the closure variable. Need to write more code. term is: ", term];
                        Return[Null];
                        ,
                        Print["CRITICAL ERROR: caseAverage: Really not expecting this option... term is: ", term];
                        Return[Null];
                    ];
                    
                    ,
                    Print["CRITICAL ERROR: caseAverage: Unable to identify newargumentparts. Term is: ", term];
                    Return[Null];
                ];
            ];
            
            If[chosen2 == 0,
                Print["CRITICAL ERROR: caseAverage: Unable to identify the number of terms coming back. Term is: ", term];
                Return[Null];
            ];
            
        ];
        
        Return[dimlesscoef];
    ];


getterminfo[term_, isdependenton_, scalars_, vectors_, tensors_, closurevar_, graddenoters_, \
divdenoters_, avgdenoters_, derivativedenoters_, dotdenoters_, tensorproductdenoters_, doubledotdenoters_, pietrzykflipdenoters_, \
divform_, gradform_, normalvecform_, Uform_, slowIV_, fastIV_, levelcountdummy_, option_] :=
    Block[{dimlesscoef, levelcount = levelcountdummy + 1, chosen = 0, \
    dependencies, i, flagsubscript = 0},
        
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
                    Print["subsubcaseSymbol: a variables was found to not dependent on all independent variables in 'isdependenton', which are", isdependenton,". Assuming it is a function of the independent variables, in general. Term is: ", term2];
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
    expandedterm = Expand[term], expandedtermparts, newinthepower = 0, chosen2 = 0},
        
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
            For[i = 1, i <= Length[dimlesscoef[[1, 1]]], i++,
                dimlesscoef[[1, 1, i]] = dimlesscoef[[1, 1, i]]^power;
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
    camebackSum = {}, newterm},
    
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
                    dropindices = Join[dropindices, {i}];
                    dimlesscoef[[1, 1]] = Join[dimlesscoef[[1, 1]], \
                      {dimlesscoef[[1, 1, 1, i]]}];
                    dimlesscoef[[1, 2]] = Join[dimlesscoef[[1, 2]], \
                      {dimlesscoef[[1, 2, 1, i]]}];
                    dimlesscoef[[1, 3]] = Join[dimlesscoef[[1, 3]], \
                      {dimlesscoef[[1, 3, 1, i]]}];
                ];
            ];
            
            If[Length[dropindices] > 0,
                dimlesscoef[[1, 1, 1]] = Drop[dimlesscoef[[1, 1, 1]], dropindices];
                dimlesscoef[[1, 2, 1]] = Drop[dimlesscoef[[1, 2, 1]], dropindices];
                dimlesscoef[[1, 3, 1]] = Drop[dimlesscoef[[1, 3, 1]], dropindices];
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

