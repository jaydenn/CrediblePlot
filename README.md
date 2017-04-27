# CrediblePlot
This mathematica package facilitates the visualization of quick and attractive 1D and 2D marginal posterior distributions that have been produced by Multinest.

The package contains 4 (hopefully self-explanatory) functions:

CrediblePlot1D[ data1D, nbins]
- data1D must be a 2xN list of samples: {{x1,prob1},{x2,prob2},...,{xN,probN}}

LogCrediblePlot1D[ data1D, nbins]
- data1D must be a 2xN list of samples: {{x1,prob1},{x2,prob2},...,{xN,probN}}

CrediblePlot2D[ data2D, nbins]
- data2D must be a 3xN list of samples: {{x1,y1,prob1},{x2,y2,prob2},...,{xN,yN,probN}}

LogLogCrediblePlot2D[ data2D, nbins]
- here data1D must be a 2xN list of samples: {{x1,y2,prob1},{x2,y2,prob2},...,{xN,yN,probN}}
