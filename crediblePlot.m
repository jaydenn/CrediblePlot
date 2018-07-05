(* ::Package:: *)

BeginPackage["crediblePlot`"]


CrediblePlot1D::usage="CrediblePlot1D[{{x1,prob1},...} , options ]"
LogCrediblePlot1D::usage="LogCrediblePlot1D[{{x1,prob1},...} , options]"
CrediblePlot2D::usage="CrediblePlot2D[{{x1,y1,prob1},...} , options]"
LogLogCrediblePlot2D::usage="LogLogCrediblePlot2D[3xN list of samples {x,y,prob} , number of bins]"
LogLinearCrediblePlot2D::usage="LogLinearCrediblePlot2D[3xN list of samples {x,y,prob} , number of bins]"
LinearLogCrediblePlot2D::usage="LinearLogCrediblePlot2D[3xN list of samples {x,y,prob} , number of bins]"
CornerPlot::usage="CornerPlot[ {list of MultiNest data}, {par1,par2,...}, options]"

LogTicksCP[min_,max_,step_] := Block[{lmin, lmax, t},
   lmin = Floor[min];
   lmax = Ceiling[max];
   t=0;
   Return[{Join[ Table[{i, If[Mod[++t,step]==0,Superscript[10, i],Null], {0.012, 0}}, {i, lmin, lmax}], 
             (Flatten[#1, 1] &)[ Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, lmin, lmax, 1}, {i, 0.1, 0.9, 0.1}]]], 
           Join[Table[{i, Null, {0.012, 0}}, {i, lmin, lmax}], 
             (Flatten[#1, 1] &)[ Table[{Log10[i*10^j], Null, {0.006, 0}}, {j, lmin, lmax, 1}, {i, 0.1, 0.9, 0.1}]]]
          }
         ];
];
LogTicksCP[min_, max_] := If[max-min>5,If[max-min>10,LogTicksCP[min,max,3],LogTicksCP[min,max,2]],LogTicksCP[min,max,1]];

CrediblePlot1D//Clear;
CrediblePlot1D[data_, opt:OptionsPattern[{CredibleLevel->{0.6827,0.9545}, NumBins->50, Smoothing->False, LoggedData->False, MaxPoint->False, SimPoint->False, CredibleColor->Blue, ListPlot}]] := 

Module[{plot, minx, maxx, xbin, binData, sum, n, cl1, cl2, interpOrder, confLimits1, confLimits2, ft, pr, maxPoint, maxPointLine},
	
	If[ Abs[Plus@@data[[All,2]]-1] > 10^-3,
	Return["Error: data not normalized, sum = "<>ToString[Plus@@data[[All,2]]]];
	];
	If[Dimensions[data][[2]]!=2,Return["List data does not have suitable dimensions"];];
	If[Length[ OptionValue[CredibleLevel] ] > 2, Return["More than 2 credible levels not currently supported"];];
	 
    If[ Depth[OptionValue[NumBins]] > 1,
        Print[OptionValue[NumBins]];
        Return["Invalid binSpec"];
        ,
        nbins = OptionValue[NumBins];
      ];
	
	minx = Min[data[[All,1]]]; 
    maxx = Max[data[[All,1]]]; 
    xbin = (maxx - minx)/nbins; 

    binData = {Table[i+xbin/2, {i, minx, maxx-xbin/2, xbin}], (Plus @@ #[[All, 2]]) & /@ (BinLists[data, {minx,maxx,xbin}, {0, 1, 1}][[All, 1]])}//Thread; 
	
	If[OptionValue[LoggedData]==True,
	    ft = { {Automatic, Automatic}, LogTicksCP[minx,maxx] },
           
        ft = Automatic;
    ];
    
	If[OptionValue[Smoothing]==True,
	interpOrder = 2;,
	interpOrder = 0;
	];
	
	pr=FilterRules[{opt}, PlotRange];   
    If[pr == {},
        pr = {{minx,maxx},Full};
      ,
      If[pr[[1,2]]=={},
       pr = {{minx,maxx},Full};
      ,
       pr = pr[[1,2]];
      ];
    ];
    binSorted = Sort[binData, #1[[2]] > #2[[2]] &];

    sum=0; 
    n=1; 
    While[ sum < OptionValue[CredibleLevel][[1]] && n<Length[binSorted], sum += binSorted[[ n, 2]]; n++;];
    cl1=binSorted[[n,2]];

    If[ Length[ OptionValue[CredibleLevel] ] == 2 ,
        While[ sum < OptionValue[CredibleLevel][[2]] && n<Length[binSorted], sum += binSorted[[ n, 2]]; n++;];
        cl2=binSorted[[n,2]];
    ];

    If[OptionValue[MaxPoint]==True,
    maxPoint = Plus @@ Apply[Times, data, 3];
	maxPointLine = {Directive[Blue,Dashed], Line[{{maxPoint, -.1}, {maxPoint, 1}}]},
	maxPointLine = {{}};
	];
	
	If[Element[OptionValue[SimPoint],Reals],
	  maxPointLine = Append[maxPointLine,{Directive[Red,Dashed], Line[{{OptionValue[SimPoint], -.1}, {OptionValue[SimPoint], 1}}]}];
	  ,
	  maxPointLine
	];

    plot        = ListPlot[  binData[[All,2]], InterpolationOrder -> interpOrder, Joined -> True, PlotStyle -> Opacity[0.5, OptionValue[CredibleColor]], PlotRange -> pr, Frame -> True, FrameTicks->ft, DataRange -> {minx , maxx}, FrameStyle->Directive[Black,20], AspectRatio->1, ImageSize->Medium, Epilog -> maxPointLine, BaseStyle->{FontFamily->"Times"}, FilterRules[ FilterRules[{opt}, Options[ListPlot]],Except[PlotRange]] ]; 
    confLimits1 = ListPlot[(#1*UnitStep[#1 - cl1] & )[binData[[All,2]]] /. 0.->-1, PlotStyle -> None, PlotRange -> pr, InterpolationOrder -> interpOrder,  AspectRatio->1, ImageSize->Medium, Joined -> True, Filling -> Bottom, FillingStyle -> Opacity[0.3, OptionValue[CredibleColor]], DataRange -> {minx , maxx}, FrameStyle->Directive[Black,20],BaseStyle->{FontFamily->"Times"}]; 
    confLimits2 = If[Length[ OptionValue[CredibleLevel] ] == 2 , 
        ListPlot[(#1*UnitStep[#1 - cl2] & )[binData[[All,2]]] /. 0.->-1, PlotStyle -> None, PlotRange -> pr, InterpolationOrder -> interpOrder,  AspectRatio->1, ImageSize->Medium, Joined -> True, Filling -> Bottom, FillingStyle -> Opacity[0.2, OptionValue[CredibleColor]], DataRange -> {minx , maxx}, FrameStyle->Directive[Black,20],BaseStyle->{FontFamily->"Times"} ], {}];

    Return[Show[plot,confLimits1,confLimits2]]; 

]; 


LogCrediblePlot1D//Clear;
LogCrediblePlot1D[data_, opt:OptionsPattern[{CredibleLevel->{0.6827,0.9545}, NumBins->50, Smoothing->False, MaxPoint->False, SimPoint->False, LoggedData->False,ListPlot}]] := 
 Module[{confLimits1, confLimits2, cl, p, minx, miny, maxx, maxy, xbin, ybin, ydata, binData, ftlog, logData, plot, simPoint}, 
	If[Dimensions[data][[2]]!=2,Return["List data does not have suitable dimensions"];];
	
	logData = data; 
	logData[[All,1]] = Log10[logData[[All,1]]];

    pr=FilterRules[{opt}, PlotRange];   

    If[ OptionValue[SimPoint] != False,
        If[ Element[OptionValue[SimPoint],Reals],
            simPoint = Log10[OptionValue[SimPoint]];
            ,
            Print["Sim point must be a real number"];
         ];
      ];
    
    If[pr != {},
        If[ Depth[pr[[2,1]]]==2,
         pr = pr[[2,1]]//Log10;
        ];
        If[ Depth[pr[[2,1]]]==3,
         pr = Join[ Log10[ pr[[2,1,{1}]] ],pr[[2,1,{2}]] ];
        ];
    ];
    
	Return[CrediblePlot1D[ logData, NumBins->OptionValue[NumBins], CredibleLevel -> OptionValue[CredibleLevel], SimPoint->simPoint, MaxPoint->OptionValue[MaxPoint], PlotRange->pr, LoggedData->True, Smoothing->OptionValue[Smoothing], FilterRules[FilterRules[{opt}, Options[ListPlot]],Except[PlotRange]] ] ];
];


CrediblePlot2D//Clear;
CrediblePlot2D[data_, opt:OptionsPattern[{CredibleLevel -> {0.6827,0.9545}, NumBins->50, LoggedData->{False,False}, FrameTicks->False, Smoothing->False, MaxPoint->False, SimPoint->{}, ShowDensity->False, CredibleColor->Blue, ListContourPlot}]] := 
Module[{cl, p, minx, miny, maxx, maxy, xbin, ybin, xNbins, yNbins, binData, ft, ftX, ftY, contourPlot, densityPlot, dr, pr, prX, prY, maxPoint, maxPointCross}, 
	If[ Abs[Plus@@data[[All,3]]-1] > 10^-3,
	Return["Error: data not normalized"];
	];
	If[Dimensions[data][[2]]!=3,Return["List data does not have suitable dimensions"];];
	
	If[ Depth[OptionValue[NumBins]] > 1,
        If[ Length[OptionValue[NumBins]] != 2,
            Return["Length of NumBins > 2"];
            ,
            {xNbins,yNbins} = OptionValue[NumBins];
          ];
        ,
        xNbins = yNbins = OptionValue[NumBins];
      ];
	
	minx = Min[data[[All,1]]]; 
	maxx = Max[data[[All,1]]]; 
	miny = Min[data[[All,2]]]; 
	maxy = Max[data[[All,2]]]; 
	xbin = (maxx - minx)/xNbins; 
	ybin = (maxy - miny)/yNbins; 

	binData = ((BinLists[ data, {minx, maxx, xbin}, {miny, maxy, ybin}, {0, 1, 1}][[All, All, 1]] // Apply[Plus, #, {2}] & ) /. 0 -> {0, 0, 0})[[All, All, 3]]//Transpose;	

	If[OptionValue[Smoothing]==True, 
	    binData = (ArrayPad[#1, 1, 0] & )[ImageData[(GaussianFilter[#1, {2, 2}] & )[Image[binData]]]];
	    dr = {{minx - (3*xbin)/2, maxx + (3*xbin)/2}, {miny - (3*ybin)/2, maxy + (3*ybin)/2}};
	    ,
        dr = {{minx, maxx}, {miny, maxy}};
	  ];
    
    pr=FilterRules[{opt}, PlotRange];   

    If[pr == {},
        pr = {{minx,maxx},{miny,maxy},All};
        ,
        If[Length[pr[[1,2]]]==0,
         pr = {{minx,maxx},{miny,maxy},All};
        ];
        If[Depth[pr[[1,2]]]==2,
         pr=Join[{{minx,maxx}},{pr[[1,2]]},{All}]
        ];
        If[Length[pr[[1,2]]]==3,
         pr=pr[[1,2]];
        ,
            If[Length[pr[[1,2]]]==2,
             pr=Join[pr[[1,2]],{All}]
            ];
        ];
        
      ];
      
   ft = OptionValue[FrameTicks];
   If[ ft==False,
      If[OptionValue[LoggedData][[1]]==True,
	     ftX = LogTicksCP[minx,maxx];
	    ,
	     ftX = Automatic; 
	  ];
	  If[OptionValue[LoggedData][[2]]==True,
	     ftY = LogTicksCP[miny,maxy];
	    ,
	     ftY = Automatic; 
	  ];
	  ft = {ftY,ftX};
	];

	cl = ( p /. (FindRoot[Plus @@ Plus @@ (binData*UnitStep[binData - p]) == #, {p, 0, 1}] &) /@ OptionValue[CredibleLevel])//Quiet;
    
    If[OptionValue[MaxPoint]==True,
    maxPoint = {(Plus @@ Apply[Times, data[[All,{1,3}]], 3]), (Plus @@ Apply[Times, data[[All,{2,3}]], 3])};
	maxPointCross = {{Inset[Style["\[Cross]", 30], maxPoint, {Center, Center}]}},
	maxPointCross = {{}};
	];
    
    If[Element[OptionValue[SimPoint],Reals],
	maxPointCross = Append[ maxPointCross, {{ Inset[Style["\[Cross]", 30, Red], OptionValue[SimPoint], {Center, -.1}] }} ];
	];

	If[ OptionValue[ShowDensity] == True,

	contourPlot = ListContourPlot[binData, ClippingStyle -> None, PlotRange->pr, Contours -> cl, DataRange -> dr,  InterpolationOrder -> 2, ContourStyle -> {{OptionValue[CredibleColor], Dashed, Thick}, {OptionValue[CredibleColor], Thick}}, ContourShading -> None, Epilog->maxPointCross, AspectRatio->1, ImageSize->Medium, FrameTicks->ft, FrameStyle->Directive[Black,20],BaseStyle->{FontFamily->"Times"}, FilterRules[ FilterRules[{opt}, Options[ListContourPlot]], Except[PlotRange]] ];

	densityPlot = ListDensityPlot[binData, PlotRange->pr, DataRange -> dr, ColorFunction -> (Opacity[.8,RGBColor[1-#, 1-#, 1]] &)];

	Return[Show[contourPlot,densityPlot,contourPlot]];,

	Return[ ListContourPlot[binData, ClippingStyle -> None, Contours -> cl, PlotRange->pr, DataRange -> dr, InterpolationOrder -> 2, ContourStyle -> {{OptionValue[CredibleColor], Dashed, Thick}, {OptionValue[CredibleColor], Thick}}, ContourShading -> {None, Opacity[0.2, OptionValue[CredibleColor]], Opacity[0.5, OptionValue[CredibleColor]]}, FrameTicks->ft, FrameStyle->Directive[Black,20], Epilog->maxPointCross, AspectRatio->1, ImageSize->Medium, BaseStyle->{FontFamily->"Times"}, FilterRules[ FilterRules[{opt}, Options[ListContourPlot]], Except[PlotRange]] ] ];
	];

];


LogLogCrediblePlot2D//Clear;
LogLogCrediblePlot2D[data_, opt:OptionsPattern[{CredibleLevel -> {0.6827,0.9545}, NumBins->50, Smoothing->False, MaxPoint->False, SimPoint->{}, ShowDensity->False, ListContourPlot}]] := 
Module[{cl, p, minx, miny, maxx, maxy, xbin, ybin, ydata, binData, logData, ftlog, plot, logRange, simPoint}, 

If[Dimensions[data][[2]]!=3,Return["List data does not have suitable dimensions"];];

logData = data; 
logData[[All,{1, 2}]] = Log10[logData[[All,{1, 2}]]]; 

logRange = FilterRules[{opt}, PlotRange];
If[ logRange != {},
    If[Length[logRange[[1,2]]]==3,
      logRange = Join[logRange[[1,2,{1,2}]]//Log10,logRange[[1,2,{3}]]];
      ,
      logRange = logRange[[1,2]]//Log10;
     ];
     ,
      logRange = {};

 ];

If[ Element[OptionValue[SimPoint],Reals],
    simPoint = Log10[OptionValue[SimPoint]];
    ,
    simPoint = False;
  ];
  
Return[CrediblePlot2D[ logData, NumBins->OptionValue[NumBins], CredibleLevel -> OptionValue[CredibleLevel], SimPoint->simPoint, MaxPoint->OptionValue[MaxPoint], LoggedData->{True,True}, Smoothing->OptionValue[Smoothing], ShowDensity->OptionValue[ShowDensity], PlotRange->logRange, FilterRules[ FilterRules[{opt}, Options[ListContourPlot]], Except[PlotRange]] ] ];
];

LogLinearCrediblePlot2D//Clear;
LogLinearCrediblePlot2D[data_, opt:OptionsPattern[{CredibleLevel -> {0.6827,0.9545}, NumBins->50, MaxPoint->False, SimPoint->False, Smoothing->False, ShowDensity->False, ListContourPlot}]] := 
Module[{cl, p, minx, miny, maxx, maxy, xbin, ybin, ydata, binData, logData, ftlog, plot, logRange, simPoint}, 

If[Dimensions[data][[2]]!=3,Return["List data does not have suitable dimensions"];];

logData = data; 
logData[[All,1]] = Log10[logData[[All,1]]]; 

logRange=FilterRules[{opt}, PlotRange];
If[ logRange != {}, 
   If[Length[logRange[[1,2]]]==3,
      logRange = Join[logRange[[1,2,{1}]]//Log10, logRange[[1,2,{2,3}]] ];
      ,
      If[(Length[logRange[[1,2]]]==2) && (Depth[logRange[[1,2]]]>2),
      logRange = Join[logRange[[1,2,{1}]]//Log10,logRange[[1,2,{2}]]];
      ,
      logRange = logRange[[1,2]];
     ];
    ];
   ,
     logRange = {};
];

If[ Element[OptionValue[SimPoint],Reals],
    simPoint = OptionValue[SimPoint];
    simPoint[[1]]=Log10[simPoint[[1]]];
    ,
    simPoint = False;
  ];

Return[CrediblePlot2D[ logData, NumBins->OptionValue[NumBins], CredibleLevel -> OptionValue[CredibleLevel], SimPoint->simPoint, MaxPoint->OptionValue[MaxPoint], LoggedData->{True,False}, Smoothing->OptionValue[Smoothing], ShowDensity->OptionValue[ShowDensity], PlotRange->logRange, FilterRules[FilterRules[{opt}, Options[ListContourPlot]], Except[PlotRange]] ] ];
];

LinearLogCrediblePlot2D//Clear;
LinearLogCrediblePlot2D[data_, opt:OptionsPattern[{CredibleLevel -> {0.6827,0.9545}, NumBins->50, MaxPoint->False, SimPoint->False, Smoothing->False, ShowDensity->False, ListContourPlot}]] := 
Module[{cl, p, minx, miny, maxx, maxy, xbin, ybin, ydata, binData, logData, ftlog, plot, logRange, simPoint}, 

If[Dimensions[data][[2]]!=3,Return["List data does not have suitable dimensions"];];

logData = data; 
logData[[All,2]] = Log10[logData[[All,2]]]; 

logRange=FilterRules[{opt}, PlotRange];
If[ logRange != {}, 
   If[Length[logRange[[1,2]]]==3,
      logRange = Join[logRange[[1,2,{1}]], logRange[[1,2,{2}]]//Log10, logRange[[1,2,{3}]] ];
      ,
      If[(Length[logRange[[1,2]]]==2) && (Depth[logRange[[1,2]]]>2),
      logRange = Join[logRange[[1,2,{1}]],logRange[[1,2,{2}]]//Log10];
      ,
      logRange = logRange[[1,2]]//Log10;
     ];
    ];
   ,
     logRange = {};
];

If[ Element[OptionValue[SimPoint],Reals],
    simPoint = OptionValue[SimPoint];
    simPoint[[2]]=Log10[simPoint[[2]]];
    ,
    simPoint = False;
  ];

Return[CrediblePlot2D[ logData, NumBins->OptionValue[NumBins], CredibleLevel -> OptionValue[CredibleLevel], SimPoint->simPoint, MaxPoint->OptionValue[MaxPoint], LoggedData->{False,True}, Smoothing->OptionValue[Smoothing], ShowDensity->OptionValue[ShowDensity], PlotRange->logRange, FilterRules[FilterRules[{opt}, Options[ListContourPlot]], Except[PlotRange]] ] ];
];

ProfilePlot1D//Clear;
ProfilePlot1D[data_, opt:OptionsPattern[{CredibleLevel->{4}, NumBins->50, Smoothing->False, LoggedData->False, MaxPoint->False, SimPoint->False, ListPlot}]] := 

Module[{plot, minx, maxx, xbin, binData, sum, n, cl1, cl2, interpOrder, confLimits1, confLimits2, ft, pr, maxPoint, maxPointLine},
	
	If[Dimensions[data][[2]]!=2,Return["List data does not have suitable dimensions"];];
	
    If[ Depth[OptionValue[NumBins]] > 1,
        Print[OptionValue[NumBins]];
        Return["Invalid binSpec"];
        ,
        nbins = OptionValue[NumBins];
      ];
	
	minx = Min[data[[All,1]]]; 
    maxx = Max[data[[All,1]]]; 
    xbin = (maxx - minx)/nbins; 
    binData = {Table[i+xbin/2, {i, minx, maxx-xbin/2, xbin}], ((Min @ #[[All, 2]]) & /@ (BinLists[data, {minx,maxx,xbin}, {0, Max[data[[All,2]]], Max[data[[All,2]]]}][[All, 1]]))-Min[data[[All,2]]]}//Thread; 
	
	If[OptionValue[LoggedData]==True,
	    ft = { {Automatic, Automatic}, LogTicksCP[minx,maxx] },
           
        ft = Automatic;
    ];
    
	If[OptionValue[Smoothing]==True,
	interpOrder = 2;,
	interpOrder = 0;
	];
	
	pr=FilterRules[{opt}, PlotRange];   
    If[pr == {},
        pr = { {minx*0.999,maxx*1.001}, {0, Min[{ 16, Max[{3,binData[[All,2]]}] }] } };
      ,
      If[pr[[1,2]]=={},
       pr = { {minx*0.999,maxx*1.001}, {0, Min[{ 16, Max[{3,binData[[All,2]]}] }] } };
      ,
       pr = pr[[1,2]];
      ];
    ];

    If[OptionValue[MaxPoint]==True,
    maxPoint = Plus @@ Apply[Times, data, 3];
	maxPointLine = {Directive[Blue,Dashed], Line[{{maxPoint, -.1}, {maxPoint, Max[data[[All,2]]]}}]},
	maxPointLine = {{}};
	];
	
	If[Element[OptionValue[SimPoint],Reals],
	  maxPointLine = Append[maxPointLine,{Directive[Red,Dashed], Line[{{OptionValue[SimPoint], -.1}, {OptionValue[SimPoint], Max[data[[All,2]]]}}]}];
	  ,
	  maxPointLine
	];

    plot        = ListPlot[  binData[[All,2]], InterpolationOrder -> interpOrder, Joined -> True, PlotStyle -> Opacity[0.5, Blue], PlotRange -> pr, Frame -> True, FrameStyle->Directive[Black,18], FrameTicks->ft, DataRange -> {minx , maxx}, AspectRatio->1, ImageSize->Medium, Epilog -> maxPointLine, FilterRules[ FilterRules[{opt}, Options[ListPlot]],Except[PlotRange]] ]; 

    Return[plot]; 

]; 

LogProfilePlot1D//Clear;
LogProfilePlot1D[data_, opt:OptionsPattern[{CredibleLevel->{4}, NumBins->50, Smoothing->False, MaxPoint->False, SimPoint->False, LoggedData->False,ListPlot}]] := 
 Module[{confLimits1, confLimits2, cl, p, minx, miny, maxx, maxy, xbin, ybin, ydata, binData, ftlog, logData, plot, simPoint}, 
	If[Dimensions[data][[2]]!=2,Return["List data does not have suitable dimensions"];];
	
	logData = data; 
	logData[[All,1]] = Log10[logData[[All,1]]];

    pr=FilterRules[{opt}, PlotRange];   

    If[ OptionValue[SimPoint] != False,
        If[ Element[OptionValue[SimPoint],Reals],
            simPoint = Log10[OptionValue[SimPoint]];
            ,
            Print["Sim point must be a real number"];
         ];
      ];
    
    If[pr != {},
        If[ Depth[pr[[2,1]]]==2,
         pr = pr[[2,1]]//Log10;
        ];
        If[ Depth[pr[[2,1]]]==3,
         pr = Join[ Log10[ pr[[2,1,{1}]] ],pr[[2,1,{2}]] ];
        ];
    ];
    
	Return[ProfilePlot1D[ logData, NumBins->OptionValue[NumBins], CredibleLevel -> OptionValue[CredibleLevel], SimPoint->simPoint, MaxPoint->OptionValue[MaxPoint], PlotRange->pr, LoggedData->True, Smoothing->OptionValue[Smoothing], FrameStyle->Directive[Black,18], FilterRules[FilterRules[{opt}, Options[ListPlot]], Except[PlotRange]] ] ];

];

ProfilePlot2D//Clear;
ProfilePlot2D[data_, opt:OptionsPattern[{CredibleLevel -> {6}, NumBins->50, LoggedData->{False,False}, Smoothing->False, MaxPoint->False, SimPoint->False, ShowDensity->False, ListContourPlot}]] := 
Module[{cl, p, minx, miny, maxx, maxy, xbin, ybin, xNbins, yNbins, binData, ft, ftX, ftY, contourPlot, densityPlot, dr, pr, prX, prY, maxPoint, maxPointCross}, 
	If[Dimensions[data][[2]]!=3,Return["List data does not have suitable dimensions"];];
	
	If[ Depth[OptionValue[NumBins]] > 1,
        If[ Length[OptionValue[NumBins]] != 2,
            Return["Length of NumBins > 2"];
            ,
            {xNbins,yNbins} = OptionValue[NumBins];
          ];
        ,
        xNbins = yNbins = OptionValue[NumBins];
      ];
	
	minx = Min[data[[All,1]]]; 
	maxx = Max[data[[All,1]]]; 
	miny = Min[data[[All,2]]]; 
	maxy = Max[data[[All,2]]]; 
	xbin = (maxx - minx)/xNbins; 
	ybin = (maxy - miny)/yNbins; 

	binData = (BinLists[ 
     data, {minx, maxx, xbin}, {miny, maxy, ybin}, {0, 
      data[[All, 3]] // Max, data[[All, 3]] // Max}][[All, All, 1, All, 
     3]] - Min[data[[All, 3]]]) // Apply[Min, #, {2}] & // Apply[If[# == -\[Infinity], -1.1 10^-90, #] &, #, {2}] & ;	
	
	If[OptionValue[Smoothing]==True, 
	    binData = (ArrayPad[#1, 1, 0] & )[ImageData[(GaussianFilter[#1, {2, 2}] & )[Image[binData]]]];
	    dr = {{minx - (3*xbin)/2, maxx + (3*xbin)/2}, {miny - (3*ybin)/2, maxy + (3*ybin)/2}};
	    ,
        dr = {{minx, maxx}, {miny, maxy}};
	  ];
    
    pr=FilterRules[{opt}, PlotRange];   

    If[pr == {},
        pr = {{minx,maxx},{miny,maxy},Full};
        ,
        If[Length[pr[[1,2]]]==0,
         pr = {{minx,maxx},{miny,maxy},Full};
        ];
        If[Depth[pr[[1,2]]]==2,
         pr=Join[{{minx,maxx}},{pr[[1,2]]},{Full}]
        ];
        If[Length[pr[[1,2]]]==3,
         pr=pr[[1,2]];
        ,
            If[Length[pr[[1,2]]]==2,
             pr=Join[pr[[1,2]],{Full}]
            ];
        ];
        
      ];

    If[OptionValue[LoggedData][[1]]==True,
	   ftX = LogTicksCP[minx,maxx];
	   ,
	ftX = Automatic; 
	];
	If[OptionValue[LoggedData][[2]]==True,
	   ftY = LogTicksCP[miny,maxy];
	   ,
	ftY = Automatic; 
	];
	ft = {ftY,ftX};
	
    If[OptionValue[MaxPoint]==True,
    maxPoint = {(Plus @@ Apply[Times, data[[All,{1,3}]], 3]), (Plus @@ Apply[Times, data[[All,{2,3}]], 3])};
	maxPointCross = {{Inset[Style["\[Cross]", 30], maxPoint, {Center, Center}]}},
	maxPointCross = {{}};
	];
    
    If[Element[OptionValue[SimPoint],Reals],
	maxPointCross = Append[ maxPointCross, {{ Inset[Style["\[Cross]", 30, Red], OptionValue[SimPoint], {Center, -.1}] }} ];
	];

	Return[ListContourPlot[binData, ClippingStyle -> None, Contours -> OptionValue[CredibleLevel], PlotRange->pr, DataRange -> dr, InterpolationOrder -> 2, ContourStyle -> {{Blue, Dashed, Thick}, {Blue, Thick}}, ContourShading -> {None, Opacity[0.2, Blue], Opacity[0.5, Blue]}, FrameStyle->Directive[Black,18], FrameTicks->ft, Epilog->maxPointCross, AspectRatio -> 1, ImageSize->Medium, FilterRules[ FilterRules[{opt}, Options[ListContourPlot]], Except[PlotRange]] ] ];

];

LogLogProfilePlot2D//Clear;
LogLogProfilePlot2D[data_, opt:OptionsPattern[{CredibleLevel -> {6}, NumBins->50, Smoothing->False, MaxPoint->False, SimPoint->{}, ShowDensity->False, ListContourPlot}]] := 
Module[{cl, p, minx, miny, maxx, maxy, xbin, ybin, ydata, binData, logData, ftlog, plot, logRange, simPoint}, 
Print[OptionValue[CredibleLevel]];
If[Dimensions[data][[2]]!=3,Return["List data does not have suitable dimensions"];];

logData = data; 
logData[[All,{1, 2}]] = Log10[logData[[All,{1, 2}]]]; 

logRange = FilterRules[{opt}, PlotRange];
If[ logRange != {},
    If[Length[logRange[[1,2]]]==3,
      logRange = Join[logRange[[1,2,{1,2}]]//Log10,logRange[[1,2,{3}]]];
      ,
      logRange = logRange[[1,2]]//Log10;
     ];
     ,
      logRange = {};

 ];

If[ Element[OptionValue[SimPoint],Reals],

    simPoint = Log10[OptionValue[SimPoint]];
    ,
    simPoint = False;
  ];
  
Return[ProfilePlot2D[ logData, NumBins->OptionValue[NumBins], CredibleLevel -> OptionValue[CredibleLevel], SimPoint->simPoint, MaxPoint->OptionValue[MaxPoint], LoggedData->{True,True}, Smoothing->OptionValue[Smoothing], ShowDensity->OptionValue[ShowDensity], PlotRange->logRange, FilterRules[ FilterRules[{opt}, Options[ListContourPlot]], Except[PlotRange]] ] ];
];

CornerPlot//Clear;
CornerPlot[dataCP_, parList_, opt:OptionsPattern[{PlotType->"Credible", CredibleLevel -> None, ParameterScale -> Linear, NumBins -> 50, Smoothing->False, ShowDensity->False, SimPoint->0, MaxPoint->False, ListContourPlot}]] := 
  Module[{plotFuncs1D, plotFuncs2D, grid, scale, simScale},
    
    If[OptionValue[PlotType]=="Credible",
        plotFuncs1D = {CrediblePlot1D, LogCrediblePlot1D};
        plotFuncs2D = {{CrediblePlot2D, LinearLogCrediblePlot2D}, {LogLinearCrediblePlot2D, LogLogCrediblePlot2D}};
        col=1;
        If[ OptionValue[CredibleLevel] == None,
            CL = {0.6827,0.9545};
            ,
            CL = OptionValue[CredibleLevel];
          ];
        ,
        If[OptionValue[PlotType]=="Profile",
            plotFuncs1D = {ProfilePlot1D, LogProfilePlot1D};
            plotFuncs2D = {{ProfilePlot2D, LinearLogCrediblePlot2D}, {LogLinearCrediblePlot2D, LogLogProfilePlot2D}};
            col=2;  
            If[ OptionValue[CredibleLevel] == None,
            CL = {6};
            ,
            CL = OptionValue[CredibleLevel];
            ];
            ,
            Return["Unrecognized PlotType"];
          ];
     ];
    
    If[ Length[parList] > (Length[dataCP[[1]]]+2),
       Return["parameter list length doesn't match data list length"];
      ];
    
    If[ Depth[OptionValue[ParameterScale]] > 1,
        If[ Length[OptionValue[ParameterScale]] != Length[parList],
            Return["Length of ParameterScale does not match parameter list length"];
            ,
            scale = OptionValue[ParameterScale]/. {"Log" -> 2, "Linear" -> 1};
          ];
        ,
        If[ OptionValue[ParameterScale]=="Linear",
            scale = Table[1,Length[parList]];
            ,
           If[ OptionValue[ParameterScale]=="Log",
               scale = Table[2,Length[parList]];
               ,
               Return["Unknown scale spec"];
             ];
          ];
      ]; 
   
    If[ Depth[OptionValue[NumBins]] > 1,
        If[ Length[OptionValue[NumBins]] != Length[parList],
            Return["Length of NumBins does not match parameter list length"];
            ,
            bins = OptionValue[NumBins];
         ];
        ,
        bins = Table[OptionValue[NumBins],Length[parList]];
      ];

   simPoint=OptionValue[SimPoint];
   If[ simPoint == 0,
        simPoint=Table[False,{i,1,Length[parList]}];
     ]; 
   If[ Length[simPoint] != Length[parList],
        Return["Length of SimPoint does not match parameter list length"];
     ];

   grid = Table[
     If[i <= j,
      If[i == j,
       plotFuncs1D[[scale[[i]]]] @@ {dataCP[[1 ;; All, {2 + i, col}]], 
        NumBins -> bins[[i]], FrameLabel -> {parList[[i]], {"\[ScriptP]",Superscript["\[CapitalDelta]\[Chi]",2]}[[col]]}, MaxPoint->OptionValue[MaxPoint], SimPoint->simPoint[[i]], CredibleLevel -> CL, FilterRules[{opt}, Options[ListContourPlot]] 
         },
       plotFuncs2D[[scale[[i]], 
          scale[[j]]]] @@ {dataCP[[1 ;; All, {2 + i, 2 + j, col}]], 
         NumBins->bins[[{i,j}]], Smoothing -> True, 
         FrameLabel -> {parList[[i]], parList[[j]]}, MaxPoint->OptionValue[MaxPoint], SimPoint->simPoint[[{i,j}]], CredibleLevel -> CL, FilterRules[{opt}, Options[ListContourPlot]]}
       ], ""]
     , {i, 1, parList // Length}, {j, 1, parList // Length}];
   Return[GraphicsGrid[grid // Transpose, Spacings -> 0]];
];

DualCornerPlot//Clear;
DualCornerPlot[dataCP_, parList_, opt:OptionsPattern[{PlotType->"Credible", CredibleLevel -> None, ParameterScale -> Linear, NumBins -> 50, Smoothing->False, ShowDensity->False, SimPoint->0, MaxPoint->False, ListContourPlot}]] := 
  Module[{plotFuncs1D, plotFuncs2D, grid, scale, simScale, x1, x2, bins1, bins2,colors},
    colors={Blue,Red,Darker[Green],Purple};
    If[OptionValue[PlotType]=="Credible",
        plotFuncs1D = {CrediblePlot1D, LogCrediblePlot1D};
        plotFuncs2D = {{CrediblePlot2D, LinearLogCrediblePlot2D}, {LogLinearCrediblePlot2D, LogLogCrediblePlot2D}};
        col=1;
        If[ OptionValue[CredibleLevel] == None,
            CL = {0.6827,0.9545};
            ,
            CL = OptionValue[CredibleLevel];
          ];
        ,
        If[OptionValue[PlotType]=="Profile",
            plotFuncs1D = {ProfilePlot1D, LogProfilePlot1D};
            plotFuncs2D = {{ProfilePlot2D, LinearLogCrediblePlot2D}, {LogLinearCrediblePlot2D, LogLogProfilePlot2D}};
            col=2;  
            If[ OptionValue[CredibleLevel] == None,
            CL = {6};
            ,
            CL = OptionValue[CredibleLevel];
            ];
            ,
            Return["Unrecognized PlotType"];
          ];
     ];
    
    If[ Length[parList] > (Length[dataCP[[1]]]+2),
       Return["parameter list length doesn't match data list length"];
      ];
    
    If[ Depth[OptionValue[ParameterScale]] > 1,
        If[ Length[OptionValue[ParameterScale]] != Length[parList],
            Return["Length of ParameterScale does not match parameter list length"];
            ,
            scale = OptionValue[ParameterScale]/. {"Log" -> 2, "Linear" -> 1};
          ];
        ,
        If[ OptionValue[ParameterScale]=="Linear",
            scale = Table[1,Length[parList]];
            ,
           If[ OptionValue[ParameterScale]=="Log",
               scale = Table[2,Length[parList]];
               ,
               Return["Unknown scale spec"];
             ];
          ];
      ]; 
   
    If[ Depth[OptionValue[NumBins]] > 1,
        If[ Length[OptionValue[NumBins]] != Length[parList],
            Return["Length of NumBins does not match parameter list length"];
            ,
            bins1 = OptionValue[NumBins];
            bins2 = OptionValue[NumBins];
         ];
        ,
        bins1 = Table[OptionValue[NumBins],Length[parList]];
        bins2 = Table[OptionValue[NumBins],Length[parList]];
      ];
    
   Do[
        x1 = Max[dataCP[[1,All,2+i]]] - Min[dataCP[[1,All,2+i]]];
        x2 = Max[dataCP[[2,All,2+i]]] - Min[dataCP[[2,All,2+i]]];
        If[ x1 > x2 ,
            bins2[[i]] = (bins1[[i]] * x2/x1)//Round;
            ,
            bins1[[i]] = (bins2[[i]] * x1/x2)//Round;
          ];
   ,{i,1,Length[parList]}];
    
   simPoint=OptionValue[SimPoint];
   If[ simPoint == 0,
        simPoint=Table[False,{i,1,Length[parList]}];
     ]; 
   If[ Length[simPoint] != Length[parList],
        Return["Length of SimPoint does not match parameter list length"];
     ];
   
   grid = Table[
     If[i <= j,
      If[i == j,
        Show[
          plotFuncs1D[[scale[[i]]]] @@@ Table[{dataCP[[k,1 ;; All, {2 + i, col}]], 
          NumBins -> bins2[[i]], FrameLabel -> {parList[[i]], {"\[ScriptP]",Superscript["\[CapitalDelta]\[Chi]",2]}[[col]]}, MaxPoint->OptionValue[MaxPoint], SimPoint->simPoint[[i]], CredibleLevel -> CL, CredibleColor->colors[[k]], FilterRules[{opt}, Options[ListContourPlot]] 
           },{k,1,Length[dataCP]}]
          ]
       ,
        Show @@ (plotFuncs2D[[scale[[i]], 
          scale[[j]]]] @@@ Table[ {dataCP[[k, 1 ;; All, {2 + i, 2 + j, col}]], 
         NumBins->bins1[[{i,j}]], Smoothing -> True, 
         FrameLabel -> {parList[[i]], parList[[j]]}, MaxPoint->OptionValue[MaxPoint], SimPoint->simPoint[[{i,j}]], CredibleLevel -> CL, CredibleColor->colors[[k]], FilterRules[{opt}, Options[ListContourPlot]]}
           ,{k,1,Length[dataCP]}])
       ]
       , ""]
     , {i, 1, parList // Length}, {j, 1, parList // Length}];

   Return[GraphicsGrid[grid // Transpose, Spacings -> 0]];
];

MedianCP[data_] := Module[{sortDat, p, i},
  sortDat = Sort[data, #1[[1]] < #2[[1]] &];
  p = 0; i = 1;
  While[p < 0.5,
   p += sortDat[[i, 2]]; i += 1;];
  Return[sortDat[[i, 1]]]];
  
CredibleInterval[data_, opt:OptionsPattern[{CredibleLevel->0.9,NumBins->0}]] := Module[{sortDat, p, i, minx, maxx, xbin, binData},
  
  If[OptionValue[NumBins] == 0,
     sortDat = Sort[data, #1[[2]] < #2[[2]] &];
     p = 0; i = 1;
     While[p < (1-OptionValue[CredibleLevel]),
      p += sortDat[[i, 2]]; i += 1;];
   
     sortDat=Drop[sortDat,i-1];
     Return[{Min[sortDat[[All, 1]]],Max[sortDat[[All, 1]]]}];
     ,
     minx = Min[data[[All,1]]]; 
     maxx = Max[data[[All,1]]]; 
     xbin = (maxx - minx)/OptionValue[NumBins]; 

     sortDat = {Table[i+xbin/2, {i, minx, maxx-xbin/2, xbin}], (Plus @@ #[[All, 2]]) & /@ (BinLists[data, {minx,maxx,xbin}, {0, 1, 1}][[All, 1]])}//Thread//Sort[#, #1[[2]] < #2[[2]] &] &; 
    
    p = 0; i = 1;
    While[p < (1-OptionValue[CredibleLevel]),
     p += sortDat[[i, 2]]; i += 1;];
   
   sortDat=Drop[sortDat,i-1];
   Return[{Min[sortDat[[All, 1]]]-xbin/2,Max[sortDat[[All, 1]]]+xbin/2}];
   ];
	
  ];

Print["Welcome to CrediblePlot, the available functions are: CrediblePlot1D, LogCrediblePlot1D, CrediblePlot2D, LogLogCrediblePlot2D, LogLinearCrediblePlot2D and LinearLogCrediblePlot2D. See the github page or readme for more details."];

EndPackage[];



