#!/usr/bin/env wolframscript
(* ::Package:: *)

z[\[Omega]_]:= E^(I \[Omega])
H[z_,b0_,a1_,a2_]:= b0 z^2 / (z^2 + a1 z + a2)

Manipulate[
	LogLogPlot[Abs[H[z[\[Omega]],1/Abs[H[z[If[True,0,\[Pi]]],1,a1,a2]],a1,a2]],{\[Omega],0,\[Pi]},PlotRange->{0.1,4}],
	{{a1,-0.5},-2,2},
	{{a2,1},0,3}
	]


z[\[Omega]_]:= E^(I \[Omega])
fs = 2 \[Pi]
\[Theta][fc_]:= fc 2 \[Pi] / fs
G[\[Omega]c_,R_]:=1/((1-R)Sqrt[1 - 2 R Cos[2 \[Omega]c] + R^2])
(* NMaxValue[{Abs[H1[ww,R,\[Theta][fc]]],ww>=0,ww<=\[Pi]},ww,PrecisionGoal\[Rule]2] *)
H1[\[Omega]_,R_,\[Theta]_]:=(1 z[\[Omega]]^2)/(1 z[\[Omega]]^2 - 2 R Cos[\[Theta]] z[\[Omega]] + R^2)
Manipulate[
Plot[Log10[Abs[H1[(\[Pi]/20000)(20^\[Omega]),1-1/10^R,\[Theta][(\[Pi]/20000)(20^fc)]]]/Abs[H1[(\[Pi]/20000)(20^0),1-1/10^R,\[Theta][(\[Pi]/20000)(20^fc)]]]],{\[Omega],1,Log[20,20000]},PlotRange->{-10,5}],
{{R,0},0,10},{{fc,3},1,Log[20,20000]}]



