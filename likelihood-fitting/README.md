## Run the app on your own computer

> Requires [R](https://cran.r-project.org/) and
> [RStudio](https://posit.co/download/rstudio-desktop/) installed on your computer.

The app is two R files: `likelihood.R` contains the statistics computations,
and `app.R` contains the GUI (an R Shiny app that imports `likelihood.R`).

1. Install the R packages it uses:

   ```r
   install.packages(c("shiny", "plotly"))
   ```

2. Download `app.R` and `likelihood.R` from
   [the app's folder on GitHub](https://github.com/rithwikjc/StatsNotes/tree/main/likelihood-fitting)
   into one folder.

3. Open `app.R` in RStudio and click **Run App**, or run:

   ```r
   shiny::runApp("path/to/that/folder")
   ```
