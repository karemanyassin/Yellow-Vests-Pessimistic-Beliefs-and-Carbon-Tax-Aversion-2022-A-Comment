# Replication package for Yellow Vests, Pessimistic Beliefs, and Carbon Tax Aversion (2022): A Comment (Rivers, Woerman, and Yassin 2023)

This code generates the results of [**Yellow Vests, Pessimistic Beliefs, and Carbon Tax Aversion (2022): A Comment (Rivers, Woerman, and Yassin 2023)**](https://www.rwi-essen.de/en/i4r-discussion-papers-series-1/yellow-vests-pessimistic-beliefs-and-carbon-tax-23090701), which is a partial replication of [**Yellow Vests, Pessimistic Beliefs, and Carbon Tax Aversion (Douenne and Fabre 2022)**](https://www.aeaweb.org/articles?id=10.1257/pol.20200092)

This package contains two elements:
* `yellow_vests_replication_package/original_data/s_original.csv`: The processed dataset used in the original paper. To generate this dataset, we first ran `preparation.R` in the original paper's replication package (available at [https://www.openicpsr.org/openicpsr/project/128143/version/V1/view](https://www.openicpsr.org/openicpsr/project/128143/version/V1/view)) and then saved the resulting dataset using `write_csv(s, "s_original.csv")`.
* `yellow_vests_replication_package/code/replication_yellow_vests.R`: The R script that generates all results in this comment. Tables and figures are output to the folder `yellow_vests_replication_package/figures_tables`.

To replicate the results:
1. Download this package retaining the existing file structure
2. Install the packages that are loaded at the beginning of the R script 
3. Run the R script `replication_yellow_vests.R`
