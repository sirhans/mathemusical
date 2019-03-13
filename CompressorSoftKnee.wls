#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Quadradic threshold soft lower limit*)


qThreshold[x_,l_,w_]:=If[x<l-w,l,If[x>l+w,x,(- l x+(l+w+x)^2/4)/w]]
Plot[qThreshold[x,-4,1],{x,-10,1}]
Plot[qThreshold[x,0,0.25],{x,-1,1}]


(* ::Section:: *)
(*Alternative methods*)


(* ::Text:: *)
(*This one has the input offset so that the output is zero when input is h (height)*)


qThreshold2[xx_,l_,w_,h_]:=Module[{x},
x = xx-w-h;
If[x<l-w,l,If[x>l+w,x,(- l x+(l+w+x)^2/4)/w]]
]
Plot[qThreshold2[x,-4,1,1],{x,-10,1},PlotRange->All]
Plot[qThreshold2[x,0,0.05,0.05],{x,-1,1},PlotRange->{0,1}]
qThreshold2[x,l,w,h]


(* ::Text:: *)
(*Here' s a version that fixes the threshold at zero and forces the height (h) to be the same as the width (w) :*)


qThreshold3[x_,h_]:=Module[{xx},
	xx = x-2h;
	If[xx<-h,0,If[xx>h,xx,(xx+h)^2/(4h)]]
]
Plot[qThreshold3[x,0.1],{x,0,1}]
