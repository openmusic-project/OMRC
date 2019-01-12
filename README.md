# OMRC
A Rhythm Constraints library for OpenMusic

By Örjan Sandred,  © IRCAM 1999 (version 1.1), Stockholm 2002 (version 1.3-...)


-------

### Release notes

###### v1.42 01/2019

Update file encodings/merge different versions

###### v1.41 01/2005
Fixed bug in r-hierarchy

###### v1.4 12/2004

New functions:
**r-markov**
**r-quant**
**hr-quant_dev**
**hr-quant_ornaments**

###### v1.32
Fixed bug in tree->simple


###### v1.31 (09/2002)

New functions:
**pause?** (will output true if a variable is a pause)
**make-rhythm-domain** (create a rhythm domain with all durations between limits)

Updated functions:
r-hierarcy, gr-hierarcy (now with the option ignore-pauses)
r-canon, gr-canon (now with the option to scale the duration in comes in realtion to dux, i.e. a canon in double durations, half duration, etc.)

###### v1.3 19/08/2002
This update makes it possible to work with pauses in all rules/functions. 
A pause is indicated as a negativ duration. All user rule tools (and all other functions) 
understands pauses.

Only Pmc is supported in this update (not the Csolver). If you want to work with the Csolver (the 
Situation library), you have to use RC version 1.1. The csolver might work in this version if you do not use pauses.

###### v1.2 (07/2002) 
A fast update at the PRISMA meeting in Paris. This includes pauses, but some serious bugs exist.

###### v1.1 (17/12/1999) 

New functions in this update: 
**r-sync-over-barline** 
**r-pattern** 
**r-layerorder** 
**gr-layerorder** 
**get-voicenumber**

With this update there is a Tutorial section in the manual. 

The following limitations exist in the library:
* There can be maximum 800 search variables in a calculation.
* There can be maximum 2400 events in each rhythm layer (single events, not motifs).
* There can be maximum 8160 grid points handled by the r-beat-subdiv (this equals 
170 measures with the time signature 4/4 if the rule r-beat-subdiv allows the subdivisions '(1 2 3 4 5 6).

This can easily be changed in the file "Classes-and-vectors.lisp": change the parameters `*max-numberof-rhythmvalues*`, `*max-size-timesigngrid*` and `*max-numberof-variables*`.

The number of voices and layers can not be changed. Never change the parameters
`*max-numberof-layers*` and `*max-numberof-voices*` (this might create serious errors).
