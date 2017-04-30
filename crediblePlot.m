(* ::Package:: *)

BeginPackage["crediblePlot`"]


CrediblePlot1D::usage="CrediblePlot1D[2xN list of samples: {x,prob} , number of bins, options]"
LogCrediblePlot1D::usage="LogCrediblePlot1D[2xN list of samples {x,prob} , number of bins]"
CrediblePlot2D::usage="CrediblePlot2D[3xN list of samples {x,y,prob} , number of bins]"
LogLogCrediblePlot2D::usage="LogLogCrediblePlot2D[3xN list of samples {x,y,prob} , number of bins]"

CrediblePlot1D//Clear;
CrediblePlot1D[data_, nbins_, opt:OptionsPattern[{CredibleLevel->{0.6827,0.9545},ListPlot}]] := 

Module[{plot, minx, maxx, xbin, binData, sum, n, cl1, cl2},
 
    minx = Min[data[[All,1]]]; 
    maxx = Max[data[[All,1]]]; 
    xbin = (maxx - minx)/nbins; 

    binData = {Table[i+xbin/2, {i, minx, maxx-xbin/2, xbin}], (Plus @@ #[[All, 2]]) & /@ (BinLists[data, {minx,maxx,xbin}, {0, 1, 1}][[All, 1]])}//Thread; 

    plot = ListPlot[binData, InterpolationOrder -> 0, Joined -> True, PlotRange -> All, Frame -> True, DataRange -> {minx , maxx }]; 

    binSorted = Sort[binData, #1[[2]] > #2[[2]] &];

    sum=0; 
    n=1; 
    While[ sum < .68, sum += binSorted[[ n, 2]]; n++ ];
    cl1=binSorted[[n,2]];

    While[ sum < .95 && n<Length[binSorted], sum += binSorted[[ n, 2]]; n++;];
    cl2=binSorted[[n,2]];

    confLimits1 = ListPlot[(#1*UnitStep[#1 - cl1] & )[binData[[All,2]]], PlotStyle -> Dashed, InterpolationOrder -> 0, PlotRange -> {0, 1.1*Max[ data[[All,2]] ] }, Joined -> True, Filling -> Axis, DataRange -> {minx , maxx }, FilterRules[{opt},Options[ListPlot]] ]; 
        confLimits2 = ListPlot[(#1*UnitStep[#1 - cl2] & )[binData[[All,2]]], PlotStyle -> Dashed, InterpolationOrder -> 0, PlotRange -> All, Joined -> True, Filling -> Axis, DataRange -> {minx , maxx }];

    Return[Show[plot,confLimits1,confLimits2]]; 

]; 


LogCrediblePlot1D[data_, nbins_, options___] := Module[{confLimits1, confLimits2, cl, p, minx, miny, maxx, maxy, xbin, ybin, ydata, binData, ftlog, logData, plot}, 
	logData = data; 
	logData[[All,1]] = Log10[logData[[All,1]]];
	
	Return[CrediblePlot1D[logData,nbins]];
];

LogCrediblePlot1D[data_, nbins_, options___] := 
Module[{confLimits1, confLimits2, cl, p, minx, miny, maxx, maxy, xbin, ybin, ydata, binData, ftlog, logData, plot}, 
	logData = data; 
	logData[[All,1]] = Log10[logData[[All,1]]]; 
	minx = Min[logData[[All,1]]]; 
	maxx = Max[logData[[All,1]]]; 
	xbin = (maxx - minx)/nbins; 
(*  
	binData = Table[Plus @@ Select[logData, i - xbin/2 < #1[[1]] < i + xbin/2 & ][[All,2]], {i, minx - xbin/2, maxx + xbin/2, xbin}]; 
	ftlog = {{Automatic, Automatic}, {Join[Table[{i, If[maxx-minx>9&&OddQ[i],Null,Superscript[10, i]], {0.012, 0}}, {i, Floor[minx], Ceiling[maxx]}], (Flatten[#1, 1] & )[Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, Floor[minx], Ceiling[maxx], 1}, 
           {i, 0.1, 0.9, 0.1}]]], Join[Table[{i, Null, {0.012, 0}}, {i, Floor[minx], Ceiling[maxx]}], (Flatten[#1, 1] & )[Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, Floor[minx], Ceiling[maxx], 1}, {i, 0.1, 0.9, 0.1}]]]}}; 
    cl = Quiet[p /. {FindRoot[Plus @@ Plus @@ (#1*UnitStep[#1 - p] & )[binData] == 0.95, {p, 0, 1}], FindRoot[Plus @@ Plus @@ (#1*UnitStep[#1 - p] & )[binData] == 0.68, {p, 0, 1}]}]; *)
     
	plot = ListPlot[binData, InterpolationOrder -> 0, Joined -> True, PlotRange -> All, Frame -> True, FrameTicks -> ftlog, DataRange -> {minx - (3*xbin)/2, maxx + (3*xbin)/2}]; 
    confLimits1 = ListPlot[(#1*UnitStep[#1 - cl[[1]]] & )[binData], PlotStyle -> Dashed, InterpolationOrder -> 0, PlotRange -> All, Joined -> True, Filling -> Axis, DataRange -> {minx - (3*xbin)/2, maxx + (3*xbin)/2}]; 
    confLimits2 = ListPlot[(#1*UnitStep[#1 - cl[[2]]] & )[binData], PlotStyle -> Dashed, InterpolationOrder -> 0, PlotRange -> All, Joined -> True, Filling -> Axis, DataRange -> {minx - (3*xbin)/2, maxx + (3*xbin)/2}]; 

Return[Show[plot, confLimits1, confLimits2, options]];
]; 

CrediblePlot2D//Clear;
CrediblePlot2D[data_, nbins_, opt:OptionsPattern[{CredibleLevel -> {0.6827,0.9545}, Smoothing->False, ShowDensity->False, ListContourPlot}]] := 
Module[{cl, p, minx, miny, maxx, maxy, xbin, ybin, binData, ft, contourPlot, densityPlot}, 
minx = Min[data[[All,1]]]; 
maxx = Max[data[[All,1]]]; 
miny = Min[data[[All,2]]]; 
maxy = Max[data[[All,2]]]; 
xbin = (maxx - minx)/nbins; 
ybin = (maxy - miny)/nbins; 

ft = {{Join[Table[{i, Superscript[10, i]}, {i, Ceiling[miny] - 1, Floor[maxy] + 1}], Table[{j + 0.5, Null}, {j, Round[miny] - 1, Round[maxy] + 1, 1}]], None}, 
       {Join[Table[{i, Superscript[10, i]}, {i, Ceiling[minx] - 1, Floor[maxx] + 1}], Table[{j + 0.5, Null}, {j, Round[minx] - 1, Round[maxx] + 1, 1}]], None}}; 

binData=(((Plus @@ # &) /@ #)[[All, 3]] &) /@ (Flatten[#, 1] & /@ 
   BinLists[data, {minx, maxx, xbin}, {miny, maxy, ybin}, {0, 1, 1}]);

If[OptionValue[Smoothing]==True, binData = (ArrayPad[#1, 1, 0] & )[ImageData[(GaussianFilter[#1, {1, 1}] & )[Image[binData]]]];];

cl = ( p /. (FindRoot[Plus @@ Plus @@ (binData*UnitStep[binData - p]) == #, {p, 0, 1}] &) /@ OptionValue[CredibleLevel])//Quiet;

If[ OptionValue[ShowDensity] == True,

contourPlot = ListContourPlot[binData, ClippingStyle -> Black, Contours -> cl, InterpolationOrder -> 2, ContourStyle -> {{Blue, Dashed, Thick}, {Blue, Thick}}, ContourShading -> None, 
      PlotRange -> All, DataRange -> {{minx - (3*xbin)/2, maxx + (3*xbin)/2}, {miny - (3*ybin)/2, maxy + (3*ybin)/2}}, FilterRules[{opt},Options[ListContourPlot]] ];

densityPlot = ListDensityPlot[binData, PlotRange -> All, DataRange -> {{minx - (3*xbin)/2, maxx + (3*xbin)/2}, {miny - (3*ybin)/2, maxy + (3*ybin)/2}}, ColorFunction -> (RGBColor[1-#, 1-#, 1] &)];

Return[Show[contourPlot,densityPlot,contourPlot]];,

Return[ListContourPlot[binData, ClippingStyle -> Black, Contours -> cl, InterpolationOrder -> 2, ContourStyle -> {{Blue, Dashed, Thick}, {Blue, Thick}}, ContourShading -> {None, Opacity[0.2, Blue], Opacity[0.5, Blue]}, 
      PlotRange -> All, DataRange -> {{minx - (3*xbin)/2, maxx + (3*xbin)/2}, {miny - (3*ybin)/2, maxy + (3*ybin)/2}}, FilterRules[{opt}, Options[ListContourPlot]] ]];

];

];


LogLogCrediblePlot2D[data_, nbins_, options___] := Module[{cl, p, minx, miny, maxx, maxy, xbin, ybin, ydata, binData, logData, ftlog, plot}, 
logData = data; 
logData[[All,{1, 2}]] = Log10[logData[[All,{1, 2}]]]; 
minx = Min[logData[[All,1]]];
maxx = Max[logData[[All,1]]];
miny = Min[logData[[All,2]]];
maxy = Max[logData[[All,2]]];
xbin = (maxx - minx)/nbins;
ybin = (maxy - miny)/nbins;
ftlog = {{Join[Table[{i, Superscript[10, i], {0.012, 0}}, {i, Floor[miny], Ceiling[maxy]}], (Flatten[#1, 1] & )[Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, Floor[miny], Ceiling[maxy], 1}, {i, 0.1, 0.9, 0.1}]]], 
       Join[Table[{i, Null, {0.012, 0}}, {i, Floor[miny], Ceiling[maxy]}], (Flatten[#1, 1] & )[Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, Floor[miny], Ceiling[maxy], 1}, {i, 0.1, 0.9, 0.1}]]]}, 
      {Join[Table[{i, Superscript[10, i], {0.012, 0}}, {i, Floor[minx], Ceiling[maxx]}], (Flatten[#1, 1] & )[Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, Floor[minx], Ceiling[maxx], 1}, {i, 0.1, 0.9, 0.1}]]], 
       Join[Table[{i, Null, {0.012, 0}}, {i, Floor[minx], Ceiling[maxx]}], (Flatten[#1, 1] & )[Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, Floor[minx], Ceiling[maxx], 1}, {i, 0.1, 0.9, 0.1}]]]}}; 
binData = (ArrayPad[#1, 1, 0] & )[ImageData[(GaussianFilter[#1, {1, 1}] & )[Image[Table[ydata = Select[logData, j - ybin/2 < #1[[2]] < j + ybin/2 & ]; 
           Table[Plus @@ Select[ydata, i - xbin/2 < #1[[1]] < i + xbin/2 & ][[All,3]], {i, minx - xbin/2, maxx + xbin/2, xbin}], {j, miny - ybin/2, maxy + ybin/2, ybin}]]]]]; 
cl = Quiet[p /. {FindRoot[Plus @@ Plus @@ (#1*UnitStep[#1 - p] & )[binData] == 0.95, {p, 0, 1}], FindRoot[Plus @@ Plus @@ (#1*UnitStep[#1 - p] & )[binData] == 0.68, {p, 0, 1}]}]; 
plot = ListContourPlot[binData, ClippingStyle -> Black, Contours -> cl, InterpolationOrder -> 2, ContourStyle -> {{Blue, Dashed, Thick}, {Blue, Thick}}, ContourShading -> {None, Opacity[0.2, Blue], Opacity[0.5, Blue]}, 
      PlotRange -> All, DataRange -> {{minx - (3*xbin)/2, maxx + (3*xbin)/2}, {miny - (3*ybin)/2, maxy + (3*ybin)/2}}]; 
Show[plot, FrameTicks -> ftlog, options]
];

Print["Welcome to crediblePlot, the available functions are: CrediblePlot1D, LogCrediblePlot1D, CrediblePlot2D and LogLogCrediblePlot2D. See the github page or readme for more details."];

EndPackage[]
