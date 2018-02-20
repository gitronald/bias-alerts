# Suppressing the Search Engine Manipulation Effect

## Citations

If you find this code or data useful, please cite our work.

Robert Epstein, Ronald E. Robertson, David Lazer, and Christo Wilson. 2017. Suppressing the Search Engine Manipulation Effect (SEME). _Proc. ACM Hum.-Comput. Interact._ 1, 2, Article 42 (November 2017). <https://doi.org/10.1145/3134677>

```
@article{epstein-cscw-2017,
	 author = {Robert Epstein and Ronald E. Robertson and David Lazer and Christo Wilson},
	 title = {{Suppressing the Search Engine Manipulation Effect (SEME)}},
	 journal = {{Proceedings of the ACM: Human-Computer Interaction}},
	 volume = {1},
	 number = {2},
	 month = {November},
	 year = {2017},
}
```

## Code and Data Documentation

- Run `source(code/main.R)` to run all scripts and replicate results
- Data used in paper is available as `data/bias_alerts.Rda` and can be overwritten by uncommenting the save line in `code/data-randsamp`, but not all results will replicate exactly because the resampling will not be exact. Use the existing `data/bias_alerts.Rda` to replicate results from the paper.

#### Data processing

- `code/data-remove-pii.R`  
    + Remove personally identifiable information.
    + Input:  `data/bias_alerts_raw_pii.tsv` [Not publicly available]
    + Output: `data/bias_alerts_raw.tsv`

- `code/data-clean.R`  
    + Clean data of timer errors, low English fluency subjects, missing data.
    + Input:  `data/bias_alerts_raw.tsv`
    + Output: `data/bias_alerts_clean.tsv`

- `code/data-format.R`  
    + Format data types and create new variables for analysis. Save as `.Rda` file to preserve data types.
    + Rearranges and renames Timer and Click variable columns based on group assignment coded in `data/Search_Order_by_Group_Database.tsv`.
    + Input:  `data/bias_alerts_clean.tsv`
    + Output: `data/bias_alerts_format.Rda`

- `code/data-randsamp.R`  
    + Random sample cleaned data for balanced experiment, bias group, and counter-balancing group $N$'s.
    + Input:  `data/bias_alerts_format.Rda`
    + Output: `data/bias_alerts.Rda`


#### Analysis

All analysis code uses `data/bias_alerts.Rda` for data. Scripts are listed here roughly in the order in which their output appears in the manuscript.

- `code/analyze-demographics.R`
    + Generate Table 1 in Appendix, output base Latex to `/tables`.

- `code/analyze-search-metrics.R`
    + Analysis of search metrics by experiment.
    + Generate Figure 2 subfigures - search statistics by rank and SERP - and save in `/plots`.
    + Calculate significance statistics (Kolmogorov-Smirnov and Spearmans' Rho) and print to console.

- `code/analyze-candidate-metrics.R` 
    + Between- and within-group tests of candidate ratings and preferences.
    + Generate Table 2 and 3 in Appendix, save in `/tables`.
    + Generate Figure 3a and 3b and analyze significance of differences, save in `/plots`.
    + Generate significance statistics for differences and print to console.

- `code/analyze-vmp.R`
    + Analyze VMP by bias group and experiment.
    + Generate Figure 3c and save to `/plots`.

- `code/analyze-vote-shift.R`
    + Analyze vote shift by bias group and experiment.
    + Generate Figure 5 and save to `/plots`.
    + Generate Table 4 and save to `/tables`.

- `code/analyze-vmp-familiarity.R`
    + Analyze VMP by familiarity level, bias group, and experiment.
    + Generate Figure 4 and save to `/plots`.

- `code/analyze-vmp-demographics.R`
    + Analyze VMP by demographic group, bias group, and experiment.
    + Generate Figure 6 and save to `/plots`.

- `code/analyze-vmp-behavior-attitude.R`
    + Analyze VMP by search depth, initial attitude strength, bias group, and experiment.
    + Generate Figure 7 and save to `/plots`.

- `code/analyze-bias-awareness.R`
    + Define bias awareness coding scheme and tag subjects who appear to be aware.
    + Measure VMP by aware vs unaware subjects in each experiment.
    + Prints significance statistics to console.
    + Measure impact of awareness on search browsing pattern and print significance statistics to console.


#### Data notes

- Group codes:
    + Group 1: Cameron search ranking bias
    + Group 2: Miliband search ranking bias
    + Group 3: Neutral group (alternating search rankings)
- Negative numbers on the bipolar candidate preference scale `PreTestLikelyVoteBipolar` and `PostTestLikelyVoteBipolar` indicate a preference for Ed Milliband, positive numbers indicate a preference for David Cameron.
- `PreVote`, `PostVote` are coded as 1 if the subject voted for the favored candidate:
    + For Group 1, 1 if vote for Cameron, else 0
    + For Group 2, 1 if vote for Miliband, else 0
    + For Group 3, 1 if vote for Miliband, else 0

- `VoteShift` is `PostVote - PreVote`. VMP is calculated as `VoteShift / PreVote`, see functions `vmp` for calculating VMP. `vmp.data` will calculate VMP for all factor levels in a group, e.g. gender: `vmp.data(study4, "Gender")`

#### Additional information

- Please contact rer@ccs.neu.edu with any additional questions.