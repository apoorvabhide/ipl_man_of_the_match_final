This is a project to predict who the man of the match of a given cricket match will be, given the ball-by-ball match data.

Converted to a player-match level table, it becomes a classification problem.

In real world terms, the problem I'm trying to solve is this:
  The match is done. Both innings are done, we know the result and how each batsman and bowler did.
  Based on that, can I predict who the man of the match is going to be?

Dataset is the IPL dataset sourced from Kaggle. Data included is till 2017 (IPL - 10).

File list:
ipl_mom.R is the wild, wild west. You go there at your own peril. It contains all the data processing I've done as well as all the models I've tested out, not necessarily in that order.

ipl_mom_cleaned.R is friendlier. It's a cleaned version of the R script, consisting of only the code used for data wrangling and the final models used.

ipl_mom_markdown.md is your best friend. It is a report I've written out to explain my process and the findings.
