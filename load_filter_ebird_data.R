
# samplFile <- "ebd-datafile-SAMPLE/ebd_US-AL-125_202503_202503_smp_relMar-2025_sampling.txt"
# obsFile <- "ebd-datafile-SAMPLE/ebd_US-AL-125_202503_202503_smp_relMar-2025.txt"
samplFile <- ""
obsFile <- ""
samplData <- read_sampling(samplFile)
obsData <- read_ebd(obsFile)

if (!nzchar(dateStart)) dateStart <- min(samplData$observation_date)
if (!nzchar(dateEnd)) dateEnd <- max(samplData$observation_date)
if (locType=="state"){
  filt <- rlang::exprs(
      state_code %in% stateCodes,
      country_code %in% countrySelect,
      between(observation_date, dateStart, dateEnd)
    )
} else if (locType=="lat.lon"){
  filt <- rlang::exprs(
      between(latitude, latRange[1], latRange[2]),
      between(longitude, lonRange[1], lonRange[2]),
      between(observation_date, dateStart, dateEnd)
    )
}
invisible(lapply(filt, function(x) {
           if (length(x)>3) {
            cat("\n min",paste(x[[2]],eval(x[[3]]), sep=":"))
            cat("\t max",paste(x[[2]],eval(x[[4]]), sep=":"))
           } else {
             cat("\n",paste(x[[2]], eval(x[[3]]), sep=":"))
           }
  }))

if (params$prompt){
  filterTrue <- readLine(prompt="Apply filter to data? (Type Y or N):")
  filterTrue <- ifelse(filterTrue=="Y", TRUE, FALSE)
} else {
  filterTrue <- TRUE
}

if (filterTrue){
  samplData <- samplData |> filter(all_species_reported) |> filter(!!!filt)
  obsData <- obsData |> filter(all_species_reported) |> filter(!!!filt)
  obsData <- semi_join(obsData, samplData, by="checklist_id")
}