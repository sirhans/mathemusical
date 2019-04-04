#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Continuous Arg plot, from https://mathematica.stackexchange.com/questions/5782/implementing-continuous-phase-arg-function *)

SetAttributes[argPlot, HoldAll];
Options[argPlot] = Options[Plot];

argPlot[exp_, {x_, x0_, x1_}, opt : OptionsPattern[argPlot]] :=
 Module[{pts, pl},
  pl = Plot[Arg[exp], {x, x0, x1}, PlotRange -> All, 
    PlotPoints -> OptionValue[PlotPoints]];
  pts = SortBy[Cases[pl, Line[pts_] :> pts, Infinity], #[[1, 1]] &];
  pts = Reap[Fold[Module[{ptsn},
         ptsn = #2;
         ptsn[[All, 2]] -= Round[ptsn[[1, 2]] - #1, 2 Pi];
         Sow[ptsn];
         ptsn[[-1, 2]]] &, 0, pts];][[2, 1]];      
  ListLinePlot[Flatten[pts, 1], opt]]


A[z_,\[Beta]_,\[Gamma]_]:=(\[Beta] z^2 + \[Gamma](1+\[Beta])z + 1)/(z^2 + \[Gamma](1+\[Beta])z + \[Beta])
D1[z_]:=z^(-1)
\[Beta][Q_,\[Omega]c_]:=(1-Tan[\[Omega]c/(2 Q)])/(1+Tan[\[Omega]c/(2 Q)])
\[Gamma][\[Omega]c_]:=-Cos[\[Omega]c]
z[\[Omega]_]:= E^(-I \[Omega])
gainToDb[g_]:= 20 Log10[g]
dbToGain[db_]:= 10^(db/20)
Manipulate[
LogLogPlot[{Abs[(1+dbToGain[2 k] +(1-dbToGain[2 k])A[z[\[Omega]],\[Beta],\[Gamma]])dbToGain[-k](1/2)]},{\[Omega],0.01,\[Pi]},PlotRange->{dbToGain[-12],dbToGain[12]}]
,{{\[Beta],0},-1,1},{{\[Gamma],0},-1,1},{k,-12,12}]





(* ::Section:: *)
(*Equalization filter (bell) from Rusty Allred*)


Heq[z_,fc_,bw_,gain_,fs_]:=Module[{\[Alpha],\[Beta],D,b0,b1,b2,a1,a2},
\[Alpha] = Tan[\[Pi] bw / fs];
\[Beta] = -Cos[2 \[Pi] fc / fs];
D = If[gain < 1, \[Alpha] + gain, \[Alpha] + 1];
b0 = If[gain < 1, gain (1+\[Alpha]), (1 + \[Alpha] gain)]/D;
b1 = If[gain < 1, 2 \[Beta] gain, 2 \[Beta]]/D;
b2 = If[gain < 1, gain (1 - \[Alpha]), (1 - \[Alpha] gain)]/D;
a1 = If[gain < 1,2 \[Beta] gain, 2 \[Beta]]/D;
a2 = If[gain < 1, gain - \[Alpha], 1 - \[Alpha]]/D;
	
	(b0 z^2 + b1 z + b2)/(z^2 + a1 z + a2)
]

Manipulate[
LogLogPlot[{Abs[Heq[z[\[Omega]],fc,fc/Q,gainToDb[g],2\[Pi]]]},{\[Omega],0.01,\[Pi]},PlotRange->{dbToGain[-12],dbToGain[12]}]
,{{Q,2},1,4},{{fc,0.5},0,\[Pi]},{g,-12,12}]


fs = 2 \[Pi];
Manipulate[
LogPlot[
Abs[
Heq[z[\[Omega]],fc,fc/Q,dbToGain[2 g],fs] dbToGain[-g 2(fc/Q)/(fs/2)]
],{\[Omega],0,\[Pi]},PlotRange->{dbToGain[-12],dbToGain[12]}],
{fc,0.01,\[Pi] 0.999},{{Q,2},.5,4},{{g,5},-12,12}]


g1 = 6;
fc1 = 1;
Q1 = 2;
fs = 2 \[Pi];
Plot[{
NIntegrate[
Abs[
Heq[z[\[Omega]],fc1,1/2,dbToGain[2 gain],fs] 
],{\[Omega],0,\[Pi]}]/(fs/2),
(E^((gain/20)^2))
},
{gain,-12,12}]
