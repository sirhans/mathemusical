#!/usr/bin/env wolframscript
(* ::Package:: *)

(* soft-hard limiter *)
softHardLimiterUpper[x_,softLimitStart_,hardLimitY_,hardLimitX_]:=Module[{a,b,c,d},
a=(hardLimitX-2 hardLimitY+softLimitStart)/(hardLimitX-softLimitStart)^3;
b=(-2 hardLimitX^2+3 hardLimitX hardLimitY-2 hardLimitX softLimitStart+3 hardLimitY softLimitStart-2 softLimitStart^2)/(hardLimitX-softLimitStart)^3;
c=(hardLimitX (hardLimitX^2+hardLimitX softLimitStart-6 hardLimitY softLimitStart+4 softLimitStart^2))/(hardLimitX-softLimitStart)^3;
d=(softLimitStart^2 (2 hardLimitX^2-3 hardLimitX hardLimitY+hardLimitY softLimitStart))/(-hardLimitX+softLimitStart)^3;
If[x>hardLimitX,hardLimitY,
	If[x<softLimitStart,x,
		a x^3 + b x^2 + c x + d]
	]
]




(* ::Text:: *)
(*We have four boundary conditions :*)


(* f[hardLimitX] \[Equal] hardLimitY *)
(* f[softLimitStart] == softLimitStart *)
(* f'[hardLimitX] \[Equal] 0 *)
(* f'[softLimitStart] \[Equal] 1 *)

f[x_]:=a x^3 + b x^2 + c x + d
Solve[{f[hardLimitX] == hardLimitY, f[softLimitStart] == softLimitStart,f'[hardLimitX] == 0,f'[softLimitStart] == 1},{a,b,c,d}] //FullSimplify
