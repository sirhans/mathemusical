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
(*Quadratic threshold to remove glitches near zero*)


qThreshold3[x_,h_]:=Module[{xx},
	xx = x-2h;
	If[xx<-h,0,If[xx>h,xx,(xx+h)^2/(4h)]]
]
Plot[qThreshold3[x,0.05],{x,0,1}]


(* ::Section:: *)
(*Release Envelope*)


ReleaseEnvelope[X_]:=Module[{fMultiplier,Q,attackFcFast,releaseFcSlow,releaseFcFast,sampleRate,releaseFilteredSlow,releaseFilteredFast,completeEnvelopeFast,output},
sampleRate = 48000;
attackFcFast = 200;
releaseFcFast = 1.5;
releaseFcSlow = releaseFcFast/2;
Q=0.5;
fMultiplier = 2.7;

releaseFilteredFast = 
	releaseOnlySVF[
	releaseOnlySVF[
	releaseOnlySVF[
		X,
	releaseFcFast fMultiplier,Q,sampleRate],
	releaseFcFast fMultiplier,Q,sampleRate],
	releaseFcFast fMultiplier,Q,sampleRate];
	
completeEnvelopeFast = 
	attackOnlySVF[
	attackOnlySVF[
	attackOnlySVF[
		releaseFilteredFast,
	attackFcFast fMultiplier,Q,sampleRate],
	attackFcFast fMultiplier,Q,sampleRate],
	attackFcFast fMultiplier,Q,sampleRate]; 
	
releaseFilteredSlow =
	releaseOnlySVF[
		completeEnvelopeFast,
	releaseFcSlow fMultiplier,Q,sampleRate]; 

	output = releaseFilteredSlow - completeEnvelopeFast;
(*{X,releaseFilteredSlow,completeEnvelopeFast,releaseFilteredFast,output,qThreshold3[#,0.05]&/@output}*)
{output,qThreshold3[#,0.01]&/@output}
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






