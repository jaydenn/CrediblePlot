(* ::Package:: *)

BeginPackage["crediblePlot`"]


CrediblePlot1D::usage="CrediblePlot1D[2xN list of samples: {x,prob} , number of bins, options]"
LogCrediblePlot1D::usage="LogCrediblePlot1D[2xN list of samples {x,prob} , number of bins]"
CrediblePlot2D::usage="CrediblePlot2D[3xN list of samples {x,y,prob} , number of bins]"
LogLogCrediblePlot2D::usage="LogLogCrediblePlot2D[3xN list of samples {x,y,prob} , number of bins]"


CrediblePlot1D//Clear;
CrediblePlot1D[data_, nbins_, opt:OptionsPattern[{CredibleLevel->{0.6827,0.9545},Smoothing->False,LoggedData->False,ListPlot}]] := 

Module[{plot, minx, maxx, xbin, binData, sum, n, cl1, cl2, interpOrder, confLimits1, confLimits2,ft},
	
	If[ Abs[Plus@@data[[All,2]]-1] > 10^-3,
	Return["Error: data not normalized, sum = "<>ToString[Plus@@data[[All,2]]]];
	];
	If[Dimensions[data][[2]]!=2,Return["List data does not have suitable dimensions"];];
	
	minx = Min[data[[All,1]]]; 
    maxx = Max[data[[All,1]]]; 
    xbin = (maxx - minx)/nbins; 

    binData = {Table[i+xbin/2, {i, minx, maxx-xbin/2, xbin}], (Plus @@ #[[All, 2]]) & /@ (BinLists[data, {minx,maxx,xbin}, {0, 1, 1}][[All, 1]])}//Thread; 
	
	If[OptionValue[LoggedData]==True,
	    ft = {{Automatic, Automatic}, {Join[Table[{i, Superscript[10, i], {0.012, 0}}, {i, Floor[minx], Ceiling[maxx]}], (Flatten[#1, 1] & )[Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, Floor[minx], Ceiling[maxx], 1}, {i, 0.1, 0.9, 0.1}]]], 
       Join[Table[{i, Null, {0.012, 0}}, {i, Floor[minx], Ceiling[maxx]}], (Flatten[#1, 1] & )[Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, Floor[minx], Ceiling[maxx], 1}, {i, 0.1, 0.9, 0.1}]]]}},
           
        ft = Automatic;
    ];
    
	If[OptionValue[Smoothing]==True,
	interpOrder = 2;,
	interpOrder = 0;
	];
	
    plot = ListPlot[binData, InterpolationOrder -> interpOrder, Joined -> True, PlotStyle -> Opacity[0.5, Blue], PlotRange -> All, Frame -> True, FrameStyle->Large, FrameTicks->ft, DataRange -> {minx , maxx }, AspectRatio->1, ImageSize->Medium, FilterRules[{opt}, Options[ListPlot]]]; 

    binSorted = Sort[binData, #1[[2]] > #2[[2]] &];

    sum=0; 
    n=1; 
    While[ sum < .68, sum += binSorted[[ n, 2]]; n++ ];
    cl1=binSorted[[n,2]];

    While[ sum < .95 && n<Length[binSorted], sum += binSorted[[ n, 2]]; n++;];
    cl2=binSorted[[n,2]];

    confLimits1 = ListPlot[(#1*UnitStep[#1 - cl1] & )[binData[[All,2]]] /. 0.->-1, PlotStyle -> None, InterpolationOrder -> interpOrder, Joined -> True, Filling -> Bottom, FillingStyle -> Opacity[0.3, Blue], DataRange -> {minx , maxx } ]; 
    confLimits2 = ListPlot[(#1*UnitStep[#1 - cl2] & )[binData[[All,2]]] /. 0.->-1, PlotStyle -> None, InterpolationOrder -> interpOrder, Joined -> True, Filling -> Bottom, FillingStyle -> Opacity[0.2, Blue], DataRange -> {minx , maxx }];

    Return[Show[plot,confLimits1,confLimits2]]; 

]; 


LogCrediblePlot1D//Clear;
LogCrediblePlot1D[data_, nbins_, opt:OptionsPattern[CrediblePlot1D]] := Module[{confLimits1, confLimits2, cl, p, minx, miny, maxx, maxy, xbin, ybin, ydata, binData, ftlog, logData, plot}, 
	If[Dimensions[data][[2]]!=2,Return["List data does not have suitable dimensions"];];
	
	logData = data; 
	logData[[All,1]] = Log10[logData[[All,1]]];
	
	Return[CrediblePlot1D[ logData, nbins, LoggedData->True, FilterRules[{opt}, Options[CrediblePlot1D]]]];
];


CrediblePlot2D//Clear;
CrediblePlot2D[data_, nbins_, opt:OptionsPattern[{CredibleLevel -> {0.6827,0.9545}, LoggedData->False, Smoothing->False, ShowDensity->False, ListContourPlot}]] := 
Module[{cl, p, minx, miny, maxx, maxy, xbin, ybin, binData, ft, contourPlot, densityPlot}, 
	If[ Abs[Plus@@data[[All,3]]-1] > 10^-3,
	Return["Error: data not normalized"];
	];
	If[Dimensions[data][[2]]!=3,Return["List data does not have suitable dimensions"];];
	
	minx = Min[data[[All,1]]]; 
	maxx = Max[data[[All,1]]]; 
	miny = Min[data[[All,2]]]; 
	maxy = Max[data[[All,2]]]; 
	xbin = (maxx - minx)/nbins; 
	ybin = (maxy - miny)/nbins; 

	If[OptionValue[LoggedData]==True,
	ft = {{Join[Table[{i, Superscript[10, i], {0.012, 0}}, {i, Floor[miny], Ceiling[maxy]}], (Flatten[#1, 1] & )[Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, Floor[miny], Ceiling[maxy], 1}, {i, 0.1, 0.9, 0.1}]]], 
       Join[Table[{i, Null, {0.012, 0}}, {i, Floor[miny], Ceiling[maxy]}], (Flatten[#1, 1] & )[Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, Floor[miny], Ceiling[maxy], 1}, {i, 0.1, 0.9, 0.1}]]]}, 
      {Join[Table[{i, Superscript[10, i], {0.012, 0}}, {i, Floor[minx], Ceiling[maxx]}], (Flatten[#1, 1] & )[Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, Floor[minx], Ceiling[maxx], 1}, {i, 0.1, 0.9, 0.1}]]], 
       Join[Table[{i, Null, {0.012, 0}}, {i, Floor[minx], Ceiling[maxx]}], (Flatten[#1, 1] & )[Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, Floor[minx], Ceiling[maxx], 1}, {i, 0.1, 0.9, 0.1}]]]}},
       
	ft = Automatic; 
	];
	binData=(((Plus @@ # &) /@ #)[[All, 3]] &) /@ (Flatten[#, 1] & /@ BinLists[data, {minx, maxx, xbin}, {miny, maxy, ybin}, {0, 1, 1}]);

	If[OptionValue[Smoothing]==True, binData = (ArrayPad[#1, 1, 0] & )[ImageData[(GaussianFilter[#1, {1, 1}] & )[Image[binData]]]];];

	cl = ( p /. (FindRoot[Plus @@ Plus @@ (binData*UnitStep[binData - p]) == #, {p, 0, 1}] &) /@ OptionValue[CredibleLevel])//Quiet;

	If[ OptionValue[ShowDensity] == True,

	contourPlot = ListContourPlot[binData, ClippingStyle -> Black, Contours -> cl, InterpolationOrder -> 2, ContourStyle -> {{Blue, Dashed, Thick}, {Blue, Thick}}, ContourShading -> None, 
      PlotRange -> All, DataRange -> {{minx - (3*xbin)/2, maxx + (3*xbin)/2}, {miny - (3*ybin)/2, maxy + (3*ybin)/2}}, ImageSize->Medium, FrameStyle->Large, FrameTicks->ft, FilterRules[{opt}, Options[ListContourPlot]] ];

	densityPlot = ListDensityPlot[binData, PlotRange -> All, DataRange -> {{minx - (3*xbin)/2, maxx + (3*xbin)/2}, {miny - (3*ybin)/2, maxy + (3*ybin)/2}}, ColorFunction -> (Opacity[.8,RGBColor[1-#, 1-#, 1]] &)];

	Return[Show[contourPlot,densityPlot,contourPlot]];,

	Return[ListContourPlot[binData, ClippingStyle -> None, Contours -> cl, InterpolationOrder -> 2, ContourStyle -> {{Blue, Dashed, Thick}, {Blue, Thick}}, ContourShading -> {None, Opacity[0.2, Blue], Opacity[0.5, Blue]}, 
      PlotRange -> All, DataRange -> {{minx - (3*xbin)/2, maxx + (3*xbin)/2}, {miny - (3*ybin)/2, maxy + (3*ybin)/2}}, FrameStyle->Large, FrameTicks->ft, FilterRules[{opt}, Options[ListContourPlot]] ]];
	];

];


LogLogCrediblePlot2D//Clear;
LogLogCrediblePlot2D[data_, nbins_, opt:OptionsPattern[{CredibleLevel -> {0.6827,0.9545}, Smoothing->False, ShowDensity->False, ListContourPlot}]] := Module[{cl, p, minx, miny, maxx, maxy, xbin, ybin, ydata, binData, logData, ftlog, plot}, 

If[Dimensions[data][[2]]!=3,Return["List data does not have suitable dimensions"];];

logData = data; 
logData[[All,{1, 2}]] = Log10[logData[[All,{1, 2}]]]; 
 
Return[CrediblePlot2D[ logData, nbins, CredibleLevel -> OptionValue[CredibleLevel], LoggedData->True, Smoothing->OptionValue[Smoothing], ShowDensity->OptionValue[ShowDensity], FilterRules[{opt}, Options[ListPlot]]]];
];


Print["Welcome to crediblePlot, the available functions are: CrediblePlot1D, LogCrediblePlot1D, CrediblePlot2D and LogLogCrediblePlot2D. See the github page or readme for more details."];

EndPackage[]
