/*
 *
 * Task code generated by SAS Studio 3.5 
 *
 * Generated on '11/4/16, 9:01 PM' 
 * Generated by 'sasdemo' 
 * Generated on server 'LOCALHOST' 
 * Generated on SAS platform 'Linux LIN X64 2.6.32-573.26.1.el6.x86_64' 
 * Generated on SAS version '9.04.01M3P06242015' 
 * Generated on browser 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36' 
 * Generated on web client 'http://192.168.228.128/SASStudio/35/main?locale=en_US&zone=GMT-07%253A00&http%3A%2F%2F192.168.228.128%2FSASStudio%2F35%2F=' 
 *
 */

/*--Set output size--*/
ods graphics / reset imagemap;

/*--SGPLOT proc statement--*/
proc sgplot data=WORK.ELECTION_RESULTS   
   (where=(Year > 1990 and (south_dakota_counties in ('Aurora', 'Minnehaha'))));
	;

	/*--TITLE and FOOTNOTE--*/
	title 'Voter Turnout for Counties Aurora and Minnehaha';

	/*--Scatter plot settings--*/
	series x=Year y=Voter_turnout_percent / group=South_Dakota_Counties 
		transparency=0.0;

	/*--X Axis--*/
	xaxis grid;

	/*--Y Axis--*/
	yaxis grid;
run;

ods graphics / reset;
title;