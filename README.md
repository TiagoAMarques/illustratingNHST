# Illustrating hypothesis tests

An interactive R Shiny app for teaching what null-hypothesis significance tests do in a single sample and over repeated experiments.

Students can generate data under a known model, calculate a test statistic and p-value, and then repeat the same experiment many times. This makes the distinction between a single result, the null distribution, Type I error, and statistical power visible.

## Tests included

1. One-sample t-test
2. One-sample Wilcoxon signed-rank test
3. Welch two-sample t-test
4. Pearson correlation test

> **Terminology:** the one-sample rank procedure is the Wilcoxon signed-rank test. The Mann–Whitney test, also called the Wilcoxon rank-sum test, compares two independent samples.

## Features

- One generated dataset or repeated simulation
- Adjustable effect size, variability, sample size, significance level, and random seed
- Visualizations of the observed data and test statistic
- Sampling distributions and p-value distributions from repeated experiments
- Type I error when the null is true and estimated power when it is false
- A guided set of classroom investigations

## Run locally

Install Shiny once:

```r
install.packages("shiny")
```

Open `IllustratingHypothesisTests.Rproj` in RStudio and run:

```r
shiny::runApp(".")
```

The app uses only `shiny` beyond base R.

## Suggested teaching sequence

1. Set the true parameter equal to its null value.
2. Generate several single datasets and observe how the statistic and p-value change.
3. Switch to **Many experiments**. The p-values should be approximately Uniform(0,1), and approximately alpha of the experiments should reject.
4. Move the true parameter away from the null. P-values should accumulate near zero, and the rejection rate now estimates power.
5. Explore how effect size, sample size, and variability affect power.
6. In the Wilcoxon tab, compare symmetric and skewed populations and discuss the assumptions behind a signed-rank location test.

## Repository contents

| File | Purpose |
|---|---|
| `app.R` | Complete Shiny interface, simulation code, and server logic |
| `manifest.json` | R and package dependencies for Posit Connect Cloud |
| `IllustratingHypothesisTests.Rproj` | RStudio project configuration |
| `.gitignore` | Excludes local session and deployment metadata |

## Publish from GitHub

The repository is ready for GitHub-connected deployment with [Posit Connect Cloud](https://connect.posit.cloud/):

1. Upload or push all repository files to GitHub.
2. In Connect Cloud, choose **Publish → Shiny**.
3. Select the repository and its `main` branch.
4. Select `app.R` as the primary file.
5. Enable **Automatically publish on push** if desired.
6. Publish the app.

After changing package dependencies, regenerate and commit the manifest:

```r
install.packages("rsconnect")
rsconnect::writeManifest(appDir = ".")
```

## Reproducibility

Every simulation uses the displayed random seed. Pressing **Generate / repeat** advances the seed deterministically, allowing both reproducible demonstrations and fresh realizations.
