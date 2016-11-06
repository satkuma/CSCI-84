data election_results;
	infile '/folders/myfolders/SouthDakota_voting_by_percent.csv' dlm=',' firstobs = 2;
	input South_Dakota_Counties	$ Democrat	Republican	Others	Year	Voter_turnout_percent	dem_percent	rep_percent	
	others_percent County_IND WINNER $;
run;

	
