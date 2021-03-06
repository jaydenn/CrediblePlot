# CrediblePlot
This mathematica package facilitates the visualization of quick and attractive 1D and 2D marginal posterior distributions that have been produced by Multinest.

The package contains 4 (hopefully self-explanatory) functions:

CrediblePlot1D[ data1D, nbins]
- data1D must be a 2xN list of samples: {{x1,prob1},{x2,prob2},...,{xN,probN}}
- nbins is the number of bins in the x direction 

LogCrediblePlot1D[ data1D, nbins]
- data1D must be a 2xN list of samples: {{x1,prob1},{x2,prob2},...,{xN,probN}}
- nbins is the number of bins in the x direction

CrediblePlot2D[ data2D, nbins]
- data2D must be a 3xN list of samples: {{x1,y1,prob1},{x2,y2,prob2},...,{xN,yN,probN}}
- nbins is the number of bins in the x and y direction

LogLogCrediblePlot2D[ data2D, nbins]
- here data1D must be a 2xN list of samples: {{x1,y1,prob1},{x2,y2,prob2},...,{xN,yN,probN}}
- nbins is the number of bins in the x and y direction

## Things to add
- different binspec in different dimensions
- retransform log plots (right now the axes are just relabled, this breaks some option functionality)
- smart auto bin choice
