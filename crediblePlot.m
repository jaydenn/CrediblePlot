(* ::Package:: *)

BeginPackage["crediblePlot`"]


CrediblePlot1D::usage="CrediblePlot1D[2xN list of samples {x,prob} , number of bins]"
LogCrediblePlot1D::usage="LogCrediblePlot1D[2xN list of samples {x,prob} , number of bins]"
CrediblePlot2D::usage="CrediblePlot2D[3xN list of samples {x,y,prob} , number of bins]"
LogLogCrediblePlot2D::usage="LogLogCrediblePlot2D[3xN list of samples {x,y,prob} , number of bins]"


CrediblePlot1D[data_, nbins_] := 
Module[{plot, minx, maxx, xbin, binData}, 
minx = Min[data[[All,1]]]; 
maxx = Max[data[[All,1]]]; 
xbin = (maxx - minx)/nbins; 

binData = Table[{i, Plus @@ Select[data, i - xbin/2 < #1[[1]] < i + xbin/2 & ][[All,2]]}, {i, minx - xbin/2, maxx + xbin/2, xbin}]; 
plot = ListPlot[binData, Joined -> True, PlotRange -> All]; 

Return[plot]; 
]; 


LogCrediblePlot1D[data_, nbins_, options___] := 
Module[{confLimits1, confLimits2, cl, p, minx, miny, maxx, maxy, xbin, ybin, ydata, binData, ftlog, logData, plot}, 
	logData = data; 
	logData[[All,1]] = Log10[logData[[All,1]]]; 
	minx = Min[logData[[All,1]]]; 
	maxx = Max[logData[[All,1]]]; 
	xbin = (maxx - minx)/nbins; 

	binData = Table[Plus @@ Select[logData, i - xbin/2 < #1[[1]] < i + xbin/2 & ][[All,2]], {i, minx - xbin/2, maxx + xbin/2, xbin}]; 
	ftlog = {{Automatic, Automatic}, {Join[Table[{i, If[maxx-minx>9&&OddQ[i],Null,Superscript[10, i]], {0.012, 0}}, {i, Floor[minx], Ceiling[maxx]}], (Flatten[#1, 1] & )[Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, Floor[minx], Ceiling[maxx], 1}, 
           {i, 0.1, 0.9, 0.1}]]], Join[Table[{i, Null, {0.012, 0}}, {i, Floor[minx], Ceiling[maxx]}], (Flatten[#1, 1] & )[Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, Floor[minx], Ceiling[maxx], 1}, {i, 0.1, 0.9, 0.1}]]]}}; 
    cl = Quiet[p /. {FindRoot[Plus @@ Plus @@ (#1*UnitStep[#1 - p] & )[binData] == 0.95, {p, 0, 1}], FindRoot[Plus @@ Plus @@ (#1*UnitStep[#1 - p] & )[binData] == 0.68, {p, 0, 1}]}]; 
     
	plot = ListPlot[binData, InterpolationOrder -> 0, Joined -> True, PlotRange -> {0, 1.1 binData//Max}, Frame -> True, FrameTicks -> ftlog, DataRange -> {minx - (3*xbin)/2, maxx + (3*xbin)/2}]; 
    confLimits1 = ListPlot[(#1*UnitStep[#1 - cl[[1]]] & )[binData], PlotStyle -> Dashed, InterpolationOrder -> 0, PlotRange -> All, Joined -> True, Filling -> Axis, DataRange -> {minx - (3*xbin)/2, maxx + (3*xbin)/2}]; 
    confLimits2 = ListPlot[(#1*UnitStep[#1 - cl[[2]]] & )[binData], PlotStyle -> Dashed, InterpolationOrder -> 0, PlotRange -> All, Joined -> True, Filling -> Axis, DataRange -> {minx - (3*xbin)/2, maxx + (3*xbin)/2}]; 

Show[plot, confLimits1, confLimits2, options]
];


CrediblePlot2D[data_, nbins_] := Module[{cl, p, minx, miny, maxx, maxy, xbin, ybin, ydata, binData, ft}, minx = Min[data[[All,1]]]; maxx = Max[data[[All,1]]]; miny = Min[data[[All,2]]]; maxy = Max[data[[All,2]]]; 
     xbin = (maxx - minx)/nbins; ybin = (maxy - miny)/nbins; ft = {{Join[Table[{i, Superscript[10, i]}, {i, Ceiling[miny] - 1, Floor[maxy] + 1}], Table[{j + 0.5, Null}, {j, Round[miny] - 1, Round[maxy] + 1, 1}]], None}, 
       {Join[Table[{i, Superscript[10, i]}, {i, Ceiling[minx] - 1, Floor[maxx] + 1}], Table[{j + 0.5, Null}, {j, Round[minx] - 1, Round[maxx] + 1, 1}]], None}}; 
     binData = (ArrayPad[#1, 1, 0] & )[ImageData[(GaussianFilter[#1, {1, 1}] & )[Image[Table[ydata = Select[data, j - ybin/2 < #1[[2]] < j + ybin/2 & ]; 
            Table[Plus @@ Select[ydata, i - xbin/2 < #1[[1]] < i + xbin/2 & ][[All,3]], {i, minx - xbin/2, maxx + xbin/2, xbin}], {j, miny - ybin/2, maxy + ybin/2, ybin}]]]]]; 
     cl = Quiet[p /. {FindRoot[Plus @@ Plus @@ (#1*UnitStep[#1 - p] & )[binData] == 0.95, {p, 0, 1}], FindRoot[Plus @@ Plus @@ (#1*UnitStep[#1 - p] & )[binData] == 0.68, {p, 0, 1}]}]; 
     ListContourPlot[binData, ClippingStyle -> Black, Contours -> cl, InterpolationOrder -> 2, ContourStyle -> {{Blue, Dashed, Thick}, {Blue, Thick}}, ContourShading -> {None, Opacity[0.2, Blue], Opacity[0.5, Blue]}, 
      PlotRange -> All, DataRange -> {{minx - (3*xbin)/2, maxx + (3*xbin)/2}, {miny - (3*ybin)/2, maxy + (3*ybin)/2}}]];


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


EndPackage[]
