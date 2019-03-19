#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Release Filter*)


releaseOnlySVF[X_,fc_,Q_,sr_]:=Module[{prevY,i,ic1, ic2, a1, a2, a3, v1, v2, v3,g,k,attackMode,output},
output =X;
g = Tan[\[Pi] fc /sr];
k=1/Q;
a1 = 1/(1+g*(g+k));
a2=g*a1;
a3= g*a2;
prevY = 0;
attackMode = True;
ic1 = ic2 = v1 = v2 = v3 = 0;

For[i=1, i<=Length[X], i++,
If[X[[i]] > prevY, 

(* attack mode: follow input exactly *)
attackMode = True;
output[[i]] = X[[i]],

(* release mode: set gradient to zero and start from last value *)
If[attackMode,
(* if this is the first sample of the release, switch to release mode and manually update the state variables *)
(* f(i) = x, f'(i) = 0 *)
attackMode = False;ic1=0; ic2=prevY];
(* process svf *)
v3 = X[[i]]- ic2;
v1 = a1*ic1 + a2*v3;
v2 = ic2 + a2*ic1+ a3*v3;
(* update state *)
ic1 = 2*v1-ic1;
ic2 = 2*v2-ic2;
(* output *)
output[[i]] = v2;
];

prevY= output[[i]];
];

output
]


(* ::Section:: *)
(*BottomUpReleaseFilter (immediately down; slow up)*)


bottomReleaseSVF[X_,fc_,Q_,sr_]:=Module[{prevY,i,ic1, ic2, a1, a2, a3, v1, v2, v3,g,k,attackMode,output},
output =X;
g = Tan[\[Pi] fc /sr];
k=1/Q;
a1 = 1/(1+g*(g+k));
a2=g*a1;
a3= g*a2;
prevY = 0;
attackMode = True;
ic1 = ic2 = v1 = v2 = v3 = 0;

For[i=1, i<=Length[X], i++,
If[X[[i]] < prevY, 

(* attack mode: follow input exactly *)
attackMode = True;
output[[i]] = X[[i]],

(* release mode: set gradient to zero and start from last value *)
If[attackMode,
(* if this is the first sample of the release, switch to release mode and manually update the state variables *)
(* f(i) = x, f'(i) = 0 *)
attackMode = False;ic1=0; ic2=prevY];
(* process svf *)
v3 =X[[i]]- ic2;
v1 =a1*ic1 + a2*v3;
v2 = ic2 + a2*ic1+ a3*v3;
(* update state *)
ic1 = 2*v1-ic1;
ic2=2*v2-ic2;
(* output *)
output[[i]] = v2;
];

prevY= output[[i]];
];

output
]


(* ::Section:: *)
(*Attack Filter*)


attackOnlySVF[X_,fc_,Q_,sr_]:=Module[{prevY,prevY2,i,ic1, ic2, a1, a2, a3, v1, v2, v3,g,gInv,k,attackMode,output},
output =X;
g = Tan[\[Pi] fc /sr];
gInv = 1.0/g;
k=1/Q;
a1 = 1/(1+g*(g+k));
a2=g*a1;
a3= g*a2;
prevY = prevY2 =0;
attackMode = False;
ic1 = ic2 = v1 = v2 = v3 = 0;

For[i=1, i<=Length[X], i++,
If[X[[i]] > prevY, 

(* attack mode: filter *)
(* if this is the first sample of the attack, switch to attack mode *)
If[!attackMode,
attackMode = True; ic1 = 0.5*(prevY-prevY2)*gInv; ic2 = prevY;
];
(* process svf *)
v3 =X[[i]]- ic2;
v1 =a1*ic1 + a2*v3;
v2 = ic2 + a2*ic1+ a3*v3;
(* update state *)
ic1 = 2*v1-ic1;
ic2=2*v2-ic2;
(* output *)
output[[i]] = v2;
,

(* release mode: copy input to output *)
If[attackMode,
(* if this is the first sample of the release, switch to release mode and manually update the state variables *)
(* f(i) = x, f'(i) = 0 *)
attackMode = False];
output[[i]] = X[[i]];
];

prevY2 = prevY;
prevY= output[[i]];
];

output
]


(* ::Section:: *)
(*Bottom Up Attack Filter*)


bottomAttackSVF[X_,fc_,Q_,sr_]:=Module[{prevY,prevY2,i,ic1, ic2, a1, a2, a3, v1, v2, v3,g,gInv,k,attackMode,output},
output =X;
g = Tan[\[Pi] fc /sr];
gInv = 1.0/g;
k=1/Q;
a1 = 1/(1+g*(g+k));
a2=g*a1;
a3= g*a2;
prevY = prevY2 =0;
attackMode = False;
ic1 = ic2 = v1 = v2 = v3 = 0;

For[i=1, i<=Length[X], i++,
If[X[[i]] < prevY, 

(* attack mode: filter *)
(* if this is the first sample of the attack, switch to attack mode *)
If[!attackMode,
attackMode = True; ic1 = 0.5*(prevY-prevY2)*gInv; ic2 = prevY;
];
(* process svf *)
v3 =X[[i]]- ic2;
v1 =a1*ic1 + a2*v3;
v2 = ic2 + a2*ic1+ a3*v3;
(* update state *)
ic1 = 2*v1-ic1;
ic2=2*v2-ic2;
(* output *)
output[[i]] = v2;
,

(* release mode: copy input to output *)
If[attackMode,
(* if this is the first sample of the release, switch to release mode and manually update the state variables *)
(* f(i) = x, f'(i) = 0 *)
attackMode = False];
output[[i]] = X[[i]];
];

prevY2 = prevY;
prevY = output[[i]];
];

output
]


(* ::Section:: *)
(*Lowpass State Variable Filter*)


lowpassSVF[X_,fc_,Q_]:=Module[{sr,i,ic1, ic2, a1, a2, a3, v1, v2, v3,g,k,output},
output =X;
g = Tan[\[Pi] fc /sr];
k = 1/Q;
a1 = 1/(1+g*(g+k));
a2 = g*a1;
a3 = g*a2;
ic1 = ic2 = v1 = v2 = v3 = 0;
sr = 48000;

For[i=1, i<=Length[X], i++,
(* process svf *)
v3 = X[[i]] - ic2;
v1 = a1*ic1 + a2*v3;
v2 = ic2 + a2*ic1 + a3*v3;
(* update state *)
ic1 = 2*v1 - ic1;
ic2=2*v2 - ic2;
(* output *)
output[[i]] = v2;
];

output
]


(* ::Section:: *)
(*Dynamic Smoothing Filter*)


DSControlCurve[x_]:=1-(x-1)^2

DynamicSmoothing[X_,fc_,sensitivity_]:=Module[{i,sr,bandz,low1,low2,low1z,low2z,g0,gc,g,output},
	sr = 48000;
	gc = Tan[\[Pi] fc / sr];
	g0 = 2 gc / (1 + gc);
	output = X;
	low1z = low2z = low1 = low2 = 0;
(*
	For[i=1,i<=Length[X],i++,
		low1z = low1;
		low2z = low2;
		bandz = low2z - low1z;
		g = Min[DSControlCurve[g0 + sensitivity*Abs[bandz]],1.0]; 
		low2 = low2z + g*(low1z-low2z);
		low1 = N[low1z] + g*(X[[i]]-low1z); 
		output[[i]] = low2z;
	]; *)
	
	
		For[i=1,i<=Length[X],i++,
		bandz = low2z - low1z;
		g = Min[DSControlCurve[g0 + sensitivity*Abs[bandz]],1.0]; 
		low2z = low2z + g*(low1z-low2z);
		low1z = N[low1z] + g*(X[[i]]-low1z); 
		output[[i]] = low2z;
	]; 
	
	
	output
]


(* ::Subsection:: *)
(*test the dynamic smoothing filter*)


DSTestSignal = Table[SawtoothWave[5 x],{x,0,1,1/2^14}];
ListLinePlot[{DSTestSignal,DynamicSmoothing[DSTestSignal,2,2^(-4)]}]


(* ::Section:: *)
(*Quadratic threshold to remove glitches near zero*)


qThreshold3[x_,h_]:=Module[{xx},
	xx = x-2h;
	If[xx<-h,0,If[xx>h,xx,(xx+h)^2/(4h)]]
]


(* ::Section:: *)
(*Conversion from attack / release time to filter cutoff frequency*)


(* ::Text:: *)
(*The constants below were determined empirically*)


ARTimeToCutoffFrequency[time_,numStages_]:=Module[{correctionFactor},
    correctionFactor = 0.472859 + 0.434404 * numStages;
    1.0 / (time / correctionFactor)
]


(* ::Section:: *)
(*Complete Envelope*)


CompleteEnvelope[X_]:=Module[{fMultiplier,Q,attackFc,releaseFc,sampleRate,releaseFiltered,completeOutput},
sampleRate = 48000;
attackFc =200;
releaseFc = 2.7;
Q=0.5;
fMultiplier = 1.5;

releaseFiltered = 
	releaseOnlySVF[
	releaseOnlySVF[
	releaseOnlySVF[
		X,
	releaseFc fMultiplier,Q,sampleRate],
	releaseFc fMultiplier,Q,sampleRate],
	releaseFc fMultiplier,Q,sampleRate];
	
completeOutput = 
	attackOnlySVF[
	attackOnlySVF[
	attackOnlySVF[
		releaseFiltered,
	attackFc fMultiplier,Q,sampleRate],
	attackFc fMultiplier,Q,sampleRate],
	attackFc fMultiplier,Q,sampleRate]; 


{X,releaseFiltered,completeOutput}
]


(* ::Subsection:: *)
(*Complete Envelope Tests*)


sampleRate = 48000;
length = 2^16;
input = Table[0Sin[151 2\[Pi] N[x]]+Sin[20 2\[Pi] N[x]],{x,0,length/sampleRate,1/sampleRate}];
input[[2Round[Length[input]/4];;3Round[Length[input]/4]]]=0;

ListLinePlot[CompleteEnvelope[Abs[input]]]

input = Table[E^(x)Sin[20 2\[Pi] N[x]],{x,0,length/sampleRate,1/sampleRate}];
input[[2Round[Length[input]/4];;3Round[Length[input]/4]]]=0;

ListLinePlot[CompleteEnvelope[Abs[input]]]


(* ::Section:: *)
(*Bottom Complete Envelope*)


BottomCompleteEnvelope[X_]:=Module[{fMultiplier,Q,attackFc,releaseFc,sampleRate,releaseFiltered,completeOutput},
sampleRate = 48000;
releaseFc = ARTimeToCutoffFrequency[1/200,3];
attackFc = ARTimeToCutoffFrequency[1/3,3];
Q=0.5;
fMultiplier = 1.5;

releaseFiltered = 
	releaseOnlySVF[
	releaseOnlySVF[
	releaseOnlySVF[
		-X,
	releaseFc fMultiplier,Q,sampleRate],
	releaseFc fMultiplier,Q,sampleRate],
	releaseFc fMultiplier,Q,sampleRate];
	
completeOutput = 
	attackOnlySVF[
	attackOnlySVF[
	attackOnlySVF[
		releaseFiltered,
	attackFc fMultiplier,Q,sampleRate],
	attackFc fMultiplier,Q,sampleRate],
	attackFc fMultiplier,Q,sampleRate]; 


-completeOutput
]


(* ::Section:: *)
(*Release Envelope*)


ReleaseEnvelope[X_]:=Module[{Q,attackFcFast,releaseFcSlow,releaseFcFast,sampleRate,releaseFilteredSlow,releaseFilteredFast,completeEnvelopeFast,output},
sampleRate = 48000;
attackFcFast = ARTimeToCutoffFrequency[1/200,3];
releaseFcFast = ARTimeToCutoffFrequency[1/3,6];
releaseFcSlow = releaseFcFast;
Q=0.5;

releaseFilteredFast = 
	releaseOnlySVF[
	releaseOnlySVF[
	releaseOnlySVF[
	releaseOnlySVF[
	releaseOnlySVF[
	releaseOnlySVF[
		X,
	releaseFcFast,Q,sampleRate],
	releaseFcFast,Q,sampleRate],
	releaseFcFast,Q,sampleRate],
	releaseFcFast,Q,sampleRate],
	releaseFcFast,Q,sampleRate],
	releaseFcFast,Q,sampleRate];
	
completeEnvelopeFast = 
	attackOnlySVF[
	attackOnlySVF[
	attackOnlySVF[
		releaseFilteredFast,
	attackFcFast,Q,sampleRate],
	attackFcFast,Q,sampleRate],
	attackFcFast,Q,sampleRate]; 
	
releaseFilteredSlow =
	releaseOnlySVF[
		completeEnvelopeFast,
	releaseFcSlow,Q,sampleRate]; 

	output = releaseFilteredSlow - completeEnvelopeFast;
{X,releaseFilteredSlow,completeEnvelopeFast,releaseFilteredFast,output,qThreshold3[#,0.005]&/@output}
(*{output,qThreshold3[#,0.01]&/@output}*)
]


(* ::Subsection:: *)
(*Release Envelope Tests*)


sampleRate = 48000;
length = 2^16;
input = Table[Sin[151 2\[Pi] N[x]]+Sin[20 2\[Pi] N[x]],{x,0,length/sampleRate,1/sampleRate}];
input[[2Round[Length[input]/4];;3Round[Length[input]/4]]]=0;

ListLinePlot[ReleaseEnvelope[Abs[input]],PlotRange->All]

input = Table[E^(x)Sin[20 2\[Pi] N[x]],{x,0,length/sampleRate,1/sampleRate}];
input[[2Round[Length[input]/4];;3Round[Length[input]/4]]]=0;

ListLinePlot[ReleaseEnvelope[Abs[input]],PlotRange->All]

input = Table[E^(-x)Sin[20 2\[Pi] N[x]],{x,0,length/sampleRate,1/sampleRate}];
input[[2Round[Length[input]/4];;3Round[Length[input]/4]]]=0;

ListLinePlot[ReleaseEnvelope[Abs[input]],PlotRange->All]





(* ::Text:: *)
(*We can tolerate the release envelop dropping down quickly but we can not tolerate it jumping up quickly because that causes leakage of the control signal into the audio output. Should we attack filter the release envelope after the subtraction of the slow - fast, rather than before?*)


(* ::Section:: *)
(*Alternative release envelope method (attack filtering AFTER subtraction)*)


ReleaseEnvelope2[X_]:=Module[{Q,dynamicSmoothingFc,dynamicSmoothingSensitivity,fixedSmoothingFc,fixedFrequencySmoothingOutput,dynamicSmoothedOutput,attackFcSmoothing,releaseFcSlow,releaseFcSmoothing,sampleRate,releaseFilteredSlow,releaseFilteredFast,slowFastDifference,attackFilteredDifference},
sampleRate = 48000;
releaseFcSlow = ARTimeToCutoffFrequency[1/2,3];
releaseFcSmoothing = ARTimeToCutoffFrequency[1/2,3];
attackFcSmoothing = ARTimeToCutoffFrequency[1/2,3];
fixedSmoothingFc = ARTimeToCutoffFrequency[1/4,3];
dynamicSmoothingFc = ARTimeToCutoffFrequency[1/(2^(-2)),1];
dynamicSmoothingSensitivity = 2^(-4);
Q=0.5;
	
releaseFilteredSlow =
	releaseOnlySVF[
	releaseOnlySVF[
	releaseOnlySVF[
		X,
	releaseFcSlow,Q,sampleRate],
	releaseFcSlow,Q,sampleRate],
	releaseFcSlow,Q,sampleRate]; 

slowFastDifference = releaseFilteredSlow - X;

attackFilteredDifference = 
	bottomAttackSVF[
	bottomAttackSVF[
	bottomAttackSVF[
	bottomReleaseSVF[
	bottomReleaseSVF[
	bottomReleaseSVF[
		slowFastDifference,
	releaseFcSmoothing,Q,sampleRate],
	releaseFcSmoothing,Q,sampleRate],
	releaseFcSmoothing,Q,sampleRate],
	attackFcSmoothing,Q,sampleRate],
	attackFcSmoothing,Q,sampleRate],
	attackFcSmoothing,Q,sampleRate]; 

(*dynamicSmoothedOutput = DynamicSmoothing[attackFilteredDifference,dynamicSmoothingFc,dynamicSmoothingSensitivity];*)	
(*fixedFrequencySmoothingOutput = lowpassSVF[attackFilteredDifference,fixedSmoothingFc,0.5];*)
Join[{X,releaseFilteredSlow,slowFastDifference,attackFilteredDifference}]
(*{output,qThreshold3[#,0.01]&/@output}*)
]


(* ::Subsection:: *)
(*Release Envelope 2 Tests*)


sampleRate = 48000;
length = 2^16;
input = Table[Sin[133 2\[Pi] N[x]]+Sin[20 2\[Pi] N[x]],{x,0,length/sampleRate,1/sampleRate}];
input[[2Round[Length[input]/4];;3Round[Length[input]/4]]]=0;

ListLinePlot[ReleaseEnvelope2[Abs[input]],PlotRange->All]

input = Table[E^(x)Sin[20 2\[Pi] N[x]],{x,0,length/sampleRate,1/sampleRate}];
input[[2Round[Length[input]/4];;3Round[Length[input]/4]]]=0;

ListLinePlot[ReleaseEnvelope2[Abs[input]],PlotRange->All]

input = Table[E^(-x)Sin[20 2\[Pi] N[x]],{x,0,length/sampleRate,1/sampleRate}];
input[[2Round[Length[input]/4];;3Round[Length[input]/4]]]=0;

ListLinePlot[ReleaseEnvelope2[Abs[input]],PlotRange->All]








(* ::Section:: *)
(*Attack Envelope*)


AttackEnvelope[X_]:=Module[{numStages,fMultiplier,Q,smoothingFc, attackSmoothingFc, attackSlowFc,releaseFc,sampleRate,attackFilteredSlow, attackFilteredFast,output, smoothedOutput, attackSmoothedOutput},
sampleRate = 48000;
numStages = 3;
releaseFc = ARTimeToCutoffFrequency[1/2,numStages*2];
attackSlowFc = ARTimeToCutoffFrequency[1/20,numStages];
smoothingFc = ARTimeToCutoffFrequency[1/5,numStages];
attackSmoothingFc = ARTimeToCutoffFrequency[1/100,numStages];
Q=0.5;

attackFilteredFast =
releaseOnlySVF[
releaseOnlySVF[
releaseOnlySVF[
releaseOnlySVF[
releaseOnlySVF[
releaseOnlySVF[
X,
releaseFc ,Q,sampleRate],
releaseFc ,Q,sampleRate],
releaseFc ,Q,sampleRate],
releaseFc ,Q,sampleRate],
releaseFc ,Q,sampleRate],
releaseFc ,Q,sampleRate]; 

attackFilteredSlow =
attackOnlySVF[
attackOnlySVF[
attackOnlySVF[
attackFilteredFast,
attackSlowFc,Q,sampleRate],
attackSlowFc,Q,sampleRate],
attackSlowFc,Q,sampleRate]; 

output =(attackFilteredFast - attackFilteredSlow);

smoothedOutput = 
releaseOnlySVF[
releaseOnlySVF[
releaseOnlySVF[
output,
smoothingFc,Q,sampleRate],
smoothingFc,Q,sampleRate],
smoothingFc,Q,sampleRate];

(*
attackSmoothedOutput = 
attackOnlySVF[
attackOnlySVF[
attackOnlySVF[
smoothedOutput,
attackSmoothingFc,Q,sampleRate],
attackSmoothingFc,Q,sampleRate],
attackSmoothingFc,Q,sampleRate];
*)

attackSmoothedOutput = DynamicSmoothing[smoothedOutput,1.0,2^(-3)];

{X,attackFilteredFast,attackFilteredSlow,output,smoothedOutput,attackSmoothedOutput}
]


(* ::Subsection:: *)
(*Test Attack Envelope*)


sampleRate = 48000;
length = 2^16;
input = Table[Sin[155 2 \[Pi] N[x]]+Sin[30 2\[Pi] N[x]],{x,0,length/sampleRate,1/sampleRate}];
input[[2Round[Length[input]/4];;3Round[Length[input]/4]]]=0;

 AA1 = AttackEnvelope[Abs[input]];
 ListLinePlot[AA1] 
 ListLinePlot[AA1,PlotRange->{0,0.1}] 

input = Table[E^(x) Sin[15 2\[Pi] N[x]],{x,0,length/sampleRate,1/sampleRate}];
input[[2Round[Length[input]/4];;3Round[Length[input]/4]]]=0;

 ListLinePlot[AttackEnvelope[Abs[input]]] 

input = Table[E^(-x) Sin[15 2\[Pi] N[x]],{x,0,length/sampleRate,1/sampleRate}];
input[[2Round[Length[input]/4];;3Round[Length[input]/4]]]=0;

 ListLinePlot[AttackEnvelope[Abs[input]]] 



