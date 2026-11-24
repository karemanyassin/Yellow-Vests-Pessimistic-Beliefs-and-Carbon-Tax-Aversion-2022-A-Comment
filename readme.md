# Replication package for Yellow Vests, Pessimistic Beliefs, and Carbon Tax Aversion: A Comment on Douenne and Fabre (*American Economic Journal: Economic Policy*, 2022)  (Rivers, Woerman, and Yassin, forthcoming)

This code generates the results of [**Yellow Vests, Pessimistic Beliefs, and Carbon Tax Aversion A Comment on Douenne and Fabre (*American Economic Journal: Economic Policy*, 2022) (Rivers, Woerman, and Yassin, forthcoming)**](https://drive.google.com/file/d/15UvEURYGuu1Zde7d0gBgf9YTpPsxM6fP/view?usp=sharing), which is a partial reproduction of [**Yellow Vests, Pessimistic Beliefs, and Carbon Tax Aversion (Douenne and Fabre 2022)**](https://www.aeaweb.org/articles?id=10.1257/pol.20200092)

This package contains two elements:
* `yellow_vests_replication_package/original_data/s_original.csv`: The processed dataset used in the original paper. To generate this dataset, we first ran `preparation.R` in the original paper's replication package (available at [https://www.openicpsr.org/openicpsr/project/128143/version/V1/view](https://www.openicpsr.org/openicpsr/project/128143/version/V1/view)) and then saved the resulting dataset using `write_csv(s, "s_original.csv")`.
* `yellow_vests_replication_package/code/replication_yellow_vests.R`: The R script that generates all results in this comment. Tables and figures are output to the folder `yellow_vests_replication_package/figures_tables`.

To replicate the results:
1. Download this package retaining the existing file structure
2. Install the packages that are loaded at the beginning of the R script 
3. Run the R script `replication_yellow_vests.R`
