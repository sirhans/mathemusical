#!/usr/bin/env wolframscript
(* ::Package:: *)

(* load the file *)
csvData = Import["/Users/hansanderson/Library/Developer/Xcode/DerivedData/SaturatorAU-ghfrwulouzmnpidhtzmyevdzmxfd/Build/Products/Debug/arrayOut.csv"];

(* extract only the y coordinate of each point *)
audioData = #[[2]]&/@csvData;

(* plot the first few samples *)
audioData[[1;;256]]//ListLinePlot

(* create an audio object *)
soundObject = Sound[ListPlay[audioData,SampleRate->48000,SampleDepth->32]]

(* write out a file to the desktop *)
Export["~/Desktop/test.wav",soundObject,SampleRate->48000, "AudioEncoding"->"Integer32"];



