
# parallel-r-materials

<!-- badges: start -->
<!-- badges: end -->

The repository contains course materials associated with the course [Parallel R Workflows](https://r-rse-parallel-r.netlify.app/) developed by [`r-rse`](https://www.r-rse.eu/) for the University of Southampton. 

To install the course materials, use:
```r
# install.packages("usethis")
usethis::use_course("r-rse/parallel-r-materials")
```

To download the large zipped geojson file from GitHub LFS use:
```r
# install.packages("piggyback")
piggyback::pb_download(repo = "r-rse/parallel-r-materials",
                       file = "lsoa_boundaries.geojson.gz",
                       dest = "<path-to-materials>/health_data/data")
```
To unzip the downloaded the zipped geojson file use:

```r
# install.packages("R.utils")
R.utils::gunzip("<path-to-materials>/health_data/data/lsoa_boundaries.geojson.gz")
```

## Atrributions

### Digital boundaries and reference maps:

Digital boundary products and reference maps are supplied under the Open Government Licence. You must use the following copyright statements when you reproduce or use this material:

- Source: Office for National Statistics licensed under the Open Government Licence v.3.0
- Contains OS data © Crown copyright and database right [2023]
