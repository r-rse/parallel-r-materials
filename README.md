
# parallel-r-materials

<!-- badges: start -->
<!-- badges: end -->

The repository contains course materials associated with the course [Parallel R Workflows](https://parallel-r.netlify.app/) developed by [`r-rse`](https://www.r-rse.eu/) for the University of Southampton. 

To install the course materials, use:
```r
# install.packages("usethis")
usethis::use_course("r-rse/parallel-r-materials")
```

Once the project is launched, to unzip the larger zipped geojson file use:

```r
# install.packages("R.utils")
R.utils::gunzip("health_data/data/lsoa_boundaries.geojson.gz")
```

## Attributions


### ONS Data, digital boundaries and reference maps:

- Source: Office for National Statistics licensed under the Open Government Licence v.3.0
- Contains OS data © Crown copyright and database right [2023]

For more details, check the [course materials](https://r-rse-parallel-r.netlify.app/04b_health_maps.html#exercise-materials)

### NBA playoffs data

- Accessed via [nbastatR](https://github.com/abresler/nbastatR) 📦
