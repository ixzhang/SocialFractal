
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SocialFractal

The goal of SocialFractal is to provide tools for analyzing social
network data, especially for the fratal pattern in egocentric networks
and multilevel social structures.

## Installation

You can install the development version of SocialFractal like so:

``` r
# Install the package from GitHub
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
# devtools::install_github("ixzhang/SocialFractal")
```

## Example Workflow

This example demonstrates how to use SocialFractal to analyze a social
network dataset. The process involves several steps, starting from
extracting social networks from raw data and then calculating the
clumping rate. Here’s a step-by-step guide:

### Step 1: Extract Social Networks from Raw Data

The first step is to extract social networks from the raw data. The raw
data should include columns such as “id” (individuals), “time” (time
points), “x” and “y” (location coordinates), and “space” (spatial
information). Note that this step requires significant computational
resources and may take a long time to run. For demonstration purposes,
we will start from the second step. Here, we show an example to extract
the social network of the first day in replicate 1, from the location
information captured per two seconds.

#### Step 1.1: Calculate Clumping Rate

Load the SocialFractal library and the example data:

``` r
library(SocialFractal)

data(raw_data)
data(zf_ind)

# Calculate clumping rate for individuals in Replicate 1
# cr_matrix <- clumping_rate(
#   data = raw_data,
#   individuals = zf_ind$Ind_ID[zf_ind$Replicate == 1],
#   threshold = 75
# )
```

#### Step 1.2: Permute Individuals within the Same Space

Test the influence of social preference by permuting individuals within
the same space:

``` r
# Example of permutating individuals within the same space
# cr_matrix_null1 <- clumping_rate_null1(
#   data = raw_data,
#   individuals = zf_ind$Ind_ID[zf_ind$Replicate == 1],
#   threshold = 75
# )
```

#### Step 1.3: Permute Individuals Without Spatial Limitation

Test the influence of spatial preference by permuting individuals
without spatial limitation:

``` r
# Example of permutating individuals without spatial limitation
# cr_matrix_null2 <- clumping_rate_null2(
#   data = raw_data,
#   individuals = zf_ind$Ind_ID[zf_ind$Replicate == 1],
#   threshold = 75
# )
```
