#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Left and right channels from Saturator*)


(* load the file *)
csvDataL = Import["/Users/hansanderson/Library/Developer/Xcode/DerivedData/SaturatorAU-gmmpuulnqraifydyjhxtrnozfqfx/Build/Products/Debug/leftChannel.csv"];
csvDataR = Import["/Users/hansanderson/Library/Developer/Xcode/DerivedData/SaturatorAU-gmmpuulnqraifydyjhxtrnozfqfx/Build/Products/Debug/rightChannel.csv"];

(* extract only the y coordinate of each point *)
audioDataL = #[[2]]&/@csvDataL;
audioDataR = #[[2]]&/@csvDataR;

(* plot the first few samples *)
ListLinePlot[audioDataL[[1;;512]],PlotRange->All]
ListLinePlot[audioDataR[[1;;512]],PlotRange->All]

(* create an audio object *)
soundObjectL = Sound[ListPlay[audioDataL,SampleRate->48000,SampleDepth->32,PlayRange->6{-1,1}]];
soundObjectR = Sound[ListPlay[audioDataR,SampleRate->48000,SampleDepth->32,PlayRange->{-1,1}]];

(* write out a file to the desktop *)
Export["~/Desktop/testL.wav",soundObjectL,SampleRate->48000, "AudioEncoding"->"Integer32"];
Export["~/Desktop/testR.wav",soundObjectR,SampleRate->48000, "AudioEncoding"->"Integer32"];


(* ::Section:: *)
(*Impulse response and sine sweep from Saturator*)


(* load the file *)
csvDataIR = Import["/Users/hansanderson/Library/Developer/Xcode/DerivedData/SaturatorAU-gmmpuulnqraifydyjhxtrnozfqfx/Build/Products/Debug/impulseResponse.csv"];
csvDataSS = Import["/Users/hansanderson/Library/Developer/Xcode/DerivedData/SaturatorAU-gmmpuulnqraifydyjhxtrnozfqfx/Build/Products/Debug/sineSweep.csv"];

(* extract only the y coordinate of each point *)
audioDataIR = #[[2]]&/@csvDataIR;
audioDataSS = #[[2]]&/@csvDataSS;

(* plot the first few samples *)
ListLinePlot[audioDataIR[[1;;150]],PlotRange->All,InterpolationOrder->2]
ListLinePlot[audioDataSS[[1;;512]],PlotRange->All]

(* create an audio object *)
soundObjectIR = Sound[ListPlay[audioDataIR,SampleRate->48000,SampleDepth->32,PlayRange->6{-1,1}]];
soundObjectSS = Sound[ListPlay[audioDataSS,SampleRate->48000,SampleDepth->32,PlayRange->6{-1,1}]];

(* write out a file to the desktop *)
Export["~/Desktop/impulseResponse.wav",soundObjectIR,SampleRate->48000, "AudioEncoding"->"Integer32"];
Export["~/Desktop/sineSweep.wav",soundObjectSS,SampleRate->48000, "AudioEncoding"->"Integer32"];

(* take the square to get positive numbers *)
adSquared = #^2& /@ audioDataSS;

(* moving average and square root to get RMS power *)
RMSPower = Sqrt /@ MovingAverage[adSquared, 48000/20];

(* plot the RMS power of the sine sweep *)
ListPlot[RMSPower,PlotRange->{0,5}]


(* ::Section:: *)
(*test output from Saturator*)


(* load the file *)
csvDataT = Import["/Users/hansanderson/Library/Developer/Xcode/DerivedData/SaturatorAU-gmmpuulnqraifydyjhxtrnozfqfx/Build/Products/Debug/test.csv"];

(* extract only the y coordinate of each point *)
audioDataT = #[[2]]&/@csvDataT;

(* plot the first few samples *)
ListLinePlot[audioDataT[[1;;500]],PlotRange->All,InterpolationOrder->2]

(* create an audio object *)
soundObjectT = Sound[ListPlay[audioDataT,SampleRate->48000,SampleDepth->32,PlayRange->6{-1,1}]];

(* write out a file to the desktop *)
Export["~/Desktop/test.wav",soundObjectT,SampleRate->48000, "AudioEncoding"->"Integer32"];


(* ::Section:: *)
(*Impulse Response and Sine Sweep from audiofitlers project*)


(* load the file *)
csvSS = Import["/Users/hansanderson/Library/Developer/Xcode/DerivedData/AudioFiltersXcodeProject-epxnpfszkygkaecpnvbrxxxqjnll/Build/Products/Debug/sineSweepResponse.csv"];
csvIR = Import["/Users/hansanderson/Library/Developer/Xcode/DerivedData/AudioFiltersXcodeProject-epxnpfszkygkaecpnvbrxxxqjnll/Build/Products/Debug/impulseResponse.csv"];
csvSFM = Import["/Users/hansanderson/Library/Developer/Xcode/DerivedData/AudioFiltersXcodeProject-epxnpfszkygkaecpnvbrxxxqjnll/Build/Products/Debug/SFMResults.csv"];

(* extract only the y coordinate of each point *)
audioDataSS = #[[2]]&/@csvSS;
audioDataIR = #[[2]]&/@csvIR;
audioDataSFM = #[[2]]&/@csvSFM;

(* plot the first few samples *)
ListLinePlot[audioDataSS[[1;;2000]],PlotRange->All]
ListLinePlot[audioDataIR[[1;;256]],PlotRange->All]
ListLinePlot[audioDataSFM,PlotRange->All]

(* create an audio object *)
soundObjectSS = Sound[ListPlay[audioDataSS,SampleRate->48000,SampleDepth->32,PlayRange->2{-1,1}]];
soundObjectIR = Sound[ListPlay[audioDataIR,SampleRate->48000,SampleDepth->32,PlayRange->1{-1,1}]];

(* write out a file to the desktop *)
Export["~/Desktop/sineSweep.wav",soundObjectSS,SampleRate->48000, "AudioEncoding"->"Integer32"];
Export["~/Desktop/impulseResponse.wav",soundObjectIR,SampleRate->48000, "AudioEncoding"->"Integer32"];










