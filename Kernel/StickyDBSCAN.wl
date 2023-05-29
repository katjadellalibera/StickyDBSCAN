(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["KatjaDellaLibera`StickyDBSCAN`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


StickyDBSCAN;
FindFissionFusionGraph;


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


(* ::Text:: *)
(*Define your public and private symbols here:*)


determineConnections[pair_,{r1_,r2_}]:=
Module[{split,testList,final},
split=
Catenate/@Split[Split[pair,( #1<=r2&&#2<=r2||#1>r2&&#2>r2||#2===Indeterminate&)],With[{first=DeleteCases[#1,Indeterminate],second=DeleteCases[#2,Indeterminate]},first\[VectorLessEqual]r2&&second\[VectorLessEqual]r2||first\[VectorGreater]r2 &&second\[VectorGreater]r2]&];
testList=MemberQ[#,_?(Function[x,x<r1])]&/@split;
final =Catenate[MapIndexed[Table[testList[[First[#2]]],Length[#1]]&,split]]
];
StickyDBSCAN[pairdata_,{r1_,r2_},{tmin_,tmax_},rawCoordinates_]:=
Module[{cons,allEdges},
cons=Transpose[Catenate[Map[determineConnections[#,{r1,r2}]&,pairdata[[All,All,tmin;;tmax]],{2}]]];
allEdges=UndirectedEdge@@@Catenate[MapIndexed[{#2[[1]],Total[#2]}&,pairdata,{2}]];

MapIndexed[
With[{members=Flatten[Position[rawCoordinates[[All,tmin-1+First[#2]]],{__?NumericQ},{1},Heads->False]]},
ConnectedComponents[Graph[
(*pick vertexes*)
members,
(*pick connections*)
Select[Pick[allEdges,#1,True],FreeQ[Alternatives@@Complement[Range[Length[pairdata]],members]]]]]]&,cons]];
FindFissionFusionGraph[tmin_,tmax_,lonelySheep_]:=Graph[Catenate[Table[FindRulesFromTo[lonelySheep[[a]],lonelySheep[[a+1]],a],{a,tmin,tmax-1}]]];
FindRulesFromTo[first_,second_,firstindex_]:=
Module[{r},
r=Reap[
Table[If[Length[Intersection[c1,c2]]>0,(*determine if there's any overlap in the groups*)
Sow[{c1,firstindex}->{c2,firstindex+1}]];,{c1,first},{c2,second}]];
If[Flatten[DeleteCases[r,{Null},2]]==={},{},DeleteDuplicates[r[[2,1]]]
]];


(* ::Section:: *)
(*Package Footer*)


End[];
EndPackage[];
