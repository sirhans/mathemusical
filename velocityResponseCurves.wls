#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Conclusion : The only thing that needs to change to get from the Velocity Curve to its inverse is the sign of the curveControlValue*)


VelocityCurve[strength_,curveControlValue_]:= Block[{logCurveControl,strength01,curvedStrength},
    (* make the velocity curve a logarithmic scale control *)
    logCurveControl = 2^(curveControlValue);
    
    (* shift strength to [0,1] *)
    strength01 = (strength-1)/126;
    
    (* apply the velocity curve *)
    curvedStrength = strength01^logCurveControl;
    
    (* scale it back to [1,127] *)
    (curvedStrength * 126) + 1
]


Solve[VelocityCurve[s,cv]==curvedVelocity,s]

VelocityCurveInverse[strength_,curveControlValue_]:= Block[{logCurveControl,strength01,curvedStrength,t},
	(* make the velocity curve a logarithmic scale control *)
    logCurveControl = 2^(-curveControlValue);
    
    (* scale from [1,127] to [0,1] *)
    strength01 = (strength - 1)/126;
    
    (* apply the inverse velocity curve *)
    curvedStrength = strength01^(logCurveControl);
    
    (* scale back to [1,127] *)
    (curvedStrength * 126) + 1
]

Plot[VelocityCurve[s,1],{s,0,127}]
Plot[VelocityCurveInverse[s,1],{s,0,127}]
Plot[VelocityCurve[VelocityCurveInverse[s,1],1],{s,0,127}]



