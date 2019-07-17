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
Plot[{Abs[(1+dbToGain[2 k] +(1-dbToGain[2 k])A[z[\[Omega]],\[Beta],\[Gamma]])dbToGain[-k](1/2)]},{\[Omega],0.01,\[Pi]},PlotRange->{dbToGain[-12],dbToGain[12]}]
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



dbToGain[db_]:= 10^(db/20)
z[\[Omega]_]:= E^(-I \[Omega])

fs = 2 \[Pi];
Manipulate[
LogPlot[
Abs[
Heq[z[\[Omega]],fc,bw,dbToGain[g],fs]
],{\[Omega],0,\[Pi]},PlotRange->{dbToGain[-12],dbToGain[12]}],
{{fc,1},0,\[Pi]},{{bw,1},0,\[Pi]},{{g,5},-12,12}]
Manipulate[
Plot[
gainToDb[Abs[
Heq[z[\[Omega]],fc,bw,dbToGain[g],fs]
]],{\[Omega],-\[Pi],\[Pi]},PlotRange->All],
{{fc,\[Pi]/2},0,\[Pi]},{{bw,1},0,\[Pi]},{{g,5},0,1000}]


(* ::Section:: *)
(*Properties*)


(* ::Text:: *)
(*1. The bandwidth of a bell filter should be in [0,Nyquist]. Values less than zero don't make sense and values more than Nyquist alias*)
(*2. The value of the integral under the magnitude transfer function does not depend on the cutoff frequency *)
(*3. The value of the integral under the magnitude transfer function is strictly increasing as bandwidth increases*)
(*4. The value of the integral is a linear function with respect to the gain of the bell filter when bandwidth = Nyquist*)
(*5. The value of the integral is a constant when bandwidth = 0*)
(*6. The value of the integral can be roughly approximated by (bandwidth * gain / (fs/2))*)


g1 = 6;
fc1 = 1;
bw1 = \[Pi] 0.5;
fs = 2 \[Pi];
Plot[{
	Log[
		NIntegrate[
			(2/fs)Abs[
				Heq[z[\[Omega]],fc1,bw1,dbToGain[gain],fs] 
			],
		{\[Omega],0,\[Pi]}]
	],
	Log[dbToGain[gain]/0.7 +10]
},{gain,0,48}]


bellIntegral[bw_,gain_]:= NIntegrate[(1/\[Pi])Abs[Heq[z[\[Omega]],\[Pi]/4,bw,dbToGain[gain],2\[Pi]]],{\[Omega],0,\[Pi]}, AccuracyGoal->3]
bwGainIntegralTable = Table[{gain,bw,Log[bellIntegral[bw,gain]]},{gain,0,48,4},{bw,0.0,\[Pi],0.025}];
ListPointPlot3D[bwGainIntegralTable]


Manipulate[
Plot[{Log[bellIntegral[bw,gain]]/Log[dbToGain[gain]],30(gain/48)(((bw/\[Pi])^(1/2.1))*(1 - (bw/\[Pi])^(1/2.1)))^(4)+(bw/\[Pi])^(1/(1+gain/11))},{bw,0,\[Pi]},PlotRange->All],
{gain,0.001,48}
]


(* ::Text:: *)
(*So, this is the formula that approximates Log[bellIntegral]/Log[dbToGain[gainInDb]] *)
(*:*)


ApproximationForm1[bw_,gainDb_]:= 30(gainDb/48)(((bw/\[Pi])^(1/2.1))*(1 - (bw/\[Pi])^(1/2.1)))^(4)+(bw/\[Pi])^(1/(1+gainDb/11))
bwGainApproximationIntegralTable = Table[{gain,bw,ApproximationForm1[bw,gain]},{gain,0,48,4},{bw,0.0,\[Pi],0.025}];
ListPointPlot3D[bwGainApproximationIntegralTable]


(* ::Text:: *)
(*And this is the formula that approximates the integral of the magnitude transfer function / nyquist, which is 1 when gain is unity :*)


integralApproximation1[bw_,gain_]:= Exp[ApproximationForm1[bw,gain]Log[dbToGain[gain]]]
bwGainApproximationIntegralTable1 = Table[{gain,bw,Log[integralApproximation1[bw,gain]]},{gain,0,48,4},{bw,0.0,\[Pi],0.025}];
ListPointPlot3D[bwGainApproximationIntegralTable1]
bwGainApproximationIntegralTable2 = Table[{gain,bw,Log[bellIntegral[bw,gain]]},{gain,0,48,4},{bw,0.0,\[Pi],0.025}];
ListPointPlot3D[bwGainApproximationIntegralTable2]


integralApproximation1[bw,gain]//FullSimplify // CForm


(* ::Text:: *)
(*How close is the approximation on the negative gain side?*)


integralApproximation1[bw_,gain_]:= Exp[ApproximationForm1[bw,gain]Log[dbToGain[gain]]]
bwGainApproximationIntegralTable1 = Table[{gain,bw,-Log[integralApproximation1[bw,gain]]},{gain,0,48,4},{bw,0.0,\[Pi],0.025}];
ListPointPlot3D[bwGainApproximationIntegralTable1]
bwGainApproximationIntegralTable2 = Table[{gain,bw,Log[bellIntegral[bw,gain]]},{gain,0,-48,-4},{bw,0.0,\[Pi],0.025}];
ListPointPlot3D[bwGainApproximationIntegralTable2]


Manipulate[
Plot[{Log[bellIntegral[bw,-gain]]/Log[dbToGain[gain]],-((-1/2)(gain/48)(((bw/\[Pi])^(2.1))*(1 - (bw/\[Pi])^(2.1)))^(1.5)+(bw/\[Pi])^(1/(1+gain/25)))},{bw,0,\[Pi]},PlotRange->All],
{gain,0.001,48}
]


integralApproximationNegativeSide[bw_,gain_]:=Exp[-((-1/2)(-gain/48)(((bw/\[Pi])^(2.1))*(1 - (bw/\[Pi])^(2.1)))^(1.5)+(bw/\[Pi])^(1/(1-gain/25)))Log[dbToGain[-gain]]]
Manipulate[
Plot[{Log[bellIntegral[bw,gain]]/Log[dbToGain[-gain]],Log[integralApproximationNegativeSide[bw,gain]]/Log[dbToGain[-gain]]},{bw,0,\[Pi]},PlotRange->All],
{{gain,-10},-48,0}
]


integralApproximationNegativeSide[bw,gain]//FullSimplify // CForm


(* ::Section:: *)
(*Exact integration*)


z[\[Omega]_]:=E^(I \[Omega])

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

HeqBoost[z_,fc_,bw_,gain_,fs_]:=Module[{\[Alpha],\[Beta],D,b0,b1,b2,a1,a2},
\[Alpha] = Tan[\[Pi] bw / fs];
\[Beta] = -Cos[2 \[Pi] fc / fs];
D = \[Alpha] + 1;
b0 = (1 + \[Alpha] gain)/D;
b1 = 2 \[Beta]/D;
b2 = (1 - \[Alpha] gain)/D;
a1 = 2 \[Beta]/D;
a2 = 1 - \[Alpha]/D;
	(b0 z^2 + b1 z + b2)/(z^2 + a1 z + a2)
]

HeqCut[z_,fc_,bw_,gain_,fs_]:=Module[{\[Alpha],\[Beta],D,b0,b1,b2,a1,a2},
\[Alpha] = Tan[\[Pi] bw / fs];
\[Beta] = -Cos[2 \[Pi] fc / fs];
D = \[Alpha] + gain;
b0 = gain (1+\[Alpha])/D;
b1 = 2 \[Beta] gain/D;
b2 = gain (1 - \[Alpha])/D;
a1 = 2 \[Beta] gain/D;
a2 = gain - \[Alpha]/D;
	
	(b0 z^2 + b1 z + b2)/(z^2 + a1 z + a2)
]

Heq2[bw_,gain_,\[Omega]_]:=If[gain<1,HeqCut[z[\[Omega]],\[Pi]/2,bw,gain,2\[Pi]],HeqBoost[z[\[Omega]],\[Pi]/2,bw,gain,2\[Pi]]]


boostIntegral[gain_,bw_,\[Omega]_]:=Evaluate[Integrate[HeqBoost[z[\[Omega]],\[Pi]/2,bw,gain,2\[Pi]],\[Omega]]]
boostIntegral[gain,bw]
cutIntegral[gain_,bw_,\[Omega]_]:=Evaluate[Integrate[HeqCut[z[\[Omega]],\[Pi]/2,bw,gain,2\[Pi]],\[Omega]]]
cutIntegral[gain,bw]
completeIntegral[gain_,bw_,\[Omega]_]:=If[gain<1,cutIntegral[gain,bw,\[Omega]],boostIntegral[gain,bw,\[Omega]]]
numericalIntegral[gain_,bw_]:=NIntegrate[Heq[z[\[Omega]],\[Pi]/2,bw,gain,2\[Pi]],{\[Omega],0,\[Pi]}]


Hd2[gain_,bw_,\[Omega]_]:=Evaluate[D[HeqBoost[z[\[Omega]],\[Pi]/2,bw,gain,2\[Pi]],{\[Omega],2}]]
Plot[Abs[Heq[z[\[Omega]],\[Pi]/2,1,2,2\[Pi]]],{\[Omega],0,\[Pi]}]
(* Plot[Abs[Hd2[2,1,\[Omega]]],{\[Omega],0,\[Pi]}] *)
Plot[Abs[Hd2[2,1,\[Omega]]],{\[Omega],0,\[Pi]},PlotRange->All]

HNumerical[gain_,bw_]:=Module[{start=0,end=0,stepSize=0.1,total=0},
	While[end<\[Pi],
		start = end;
		end += stepSize / Abs[Hd2[gain,bw,end]];
		end = Min[end,\[Pi]];
		total += Abs[completeIntegral[gain,bw,end]]-Abs[completeIntegral[gain,bw,start]]
	];
	total
]

HNumerical[2,1]


dBToGain[db_]:=10^(db/20)
ListPlot3D[Flatten[Table[{gain,bw,Abs[completeIntegral[dBToGain[gain],bw,\[Pi]]]-Abs[completeIntegral[dBToGain[gain],bw,0]]},{bw,0,\[Pi]},{gain,-30,30}],1]]


ListPlot3D[Flatten[Table[{gain,bw,Abs[Integrate[Heq2[1,2,\[Omega]],{\[Omega],0,\[Pi]}]]},{bw,0,\[Pi]},{gain,-30,30}],1]]
