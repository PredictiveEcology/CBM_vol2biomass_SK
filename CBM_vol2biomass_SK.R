defineModule(sim, list(
  name = "CBM_vol2biomass_SK",
  description = paste("A module to prepare the user-provided growth and yield information for use",
                      "in the family of models spadesCBM - CBM-CFS3-like simulation of forest",
                      "carbon in the platform SpaDES. This module takes in user-provided m3/ha",
                      "and meta data for teh growth curves and returns annual increments for",
                      "the aboveground live c-pools."),
  keywords = "",
  authors = c(
    person("Céline", "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(CBM_vol2biomass_SK = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "CBM_vol2biomass_SK.Rmd")),
  reqdPkgs = list(
    "PredictiveEcology/CBMutils@development", "PredictiveEcology/LandR@development",
    "ggforce", "ggplot2", "ggpubr", "googledrive", "mgcv", "quickPlot", "robustbase", "data.table"
  ),
  parameters = rbind(
    defineParameter(
      "outputFigurePath", "character", NA, NA, NA,
      paste("Filepath to a directory where output figures will be saved.",
            "The default is a directory named 'CBM_vol2biomass_figures' within the simulation outputs directory.")
    ),
    defineParameter(
      ".plotInitialTime", "numeric", NA, NA, NA,
      "Describes the simulation time at which the first plot event should occur."
    ),
    defineParameter(
      ".plotInterval", "numeric", NA, NA, NA,
      "Describes the simulation time interval between plot events."
    ),
    defineParameter(
      ".saveInitialTime", "numeric", NA, NA, NA,
      "Describes the simulation time at which the first save event should occur."
    ),
    defineParameter(
      ".saveInterval", "numeric", NA, NA, NA,
      "This describes the simulation time interval between save events."
    ),
    defineParameter(
      ".useCache", "logical", TRUE, NA, NA,
      paste(
        "Should this entire module be run with caching activated?",
        "This is generally intended for data-type modules, where stochasticity",
        "and time are not relevant"
      )
    )
  ),
  inputObjects = bindrows(
    # expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    # this are variables in inputed data.tables:SpatialUnitID, EcoBoundaryID, juris_id, ecozone, jur, eco, name, gcids, plotsRawCumulativeBiomass, checkInc
    expectsInput(
      objectName = "userGcM3URL", objectClass = "character",
      desc = "URL for userGcM3"),
    expectsInput(
      objectName = "userGcM3", objectClass = "data.frame",
      desc = paste("User file containing:",
                   "`gcids`, `Age`, `MerchVolume`.",
                   "Default name `userGcM3`."),
      sourceURL = "https://drive.google.com/file/d/1u7o2BzPZ2Bo7hNcC8nEctNpDmp7ce84m"),
    expectsInput(
      objectName = "spatialUnits", objectClass = "data.table",
      desc = paste("the table linking the spu id, with the `disturbance_matrix_id` and the events.",
                   "The events are the possible raster values from the disturbance rasters of Wulder and White."),
      sourceURL = NA),
    expectsInput(
      objectName = "cbmAdmin", objectClass = "data.frame",
      desc = paste("Provides equivalent between provincial boundaries,",
                   "CBM-id for provincial boundaries and CBM-spatial unit ids"),
      sourceURL = "https://drive.google.com/file/d/1xdQt9JB5KRIw72uaN5m3iOk8e34t9dyz"),
    expectsInput(
      objectName = "cbmAdminURL", objectClass = "character",
      desc = "URL for cbmAdmin"),
    expectsInput(
      objectName = "ecozones", objectClass = "data.table",
      desc = paste("Vector, one for each stand, indicating the numeric representation",
                   "of the Canadian ecozones, as used in CBM-CFS3"),
      sourceURL = NA),
    expectsInput(
      objectName = "table3", objectClass = "data.frame",
      desc = "Stem wood biomass model parameters for merchantable-sized trees from Boudewyn et al 2007",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table3.csv"),
    expectsInput(
      objectName = "table3URL", objectClass = "character",
      desc = "URL for table 3"),
    expectsInput(
      objectName = "table4", objectClass = "data.frame", desc = "Stem wood biomass model parameters for nonmerchantable-sized trees from Boudewyn et al 2007",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table4.csv"),
    expectsInput(
      objectName = "table4URL", objectClass = "character",
      desc = "URL for table 4"),
    expectsInput(
      objectName = "table5", objectClass = "data.frame",
      desc = "Stem wood biomass model parameters for sapling-sized trees from Boudewyn et al. 2007.",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table5.csv"),
    expectsInput(
      objectName = "table5URL", objectClass = "character",
      desc = "URL for table 5"),
    expectsInput(
      objectName = "table6", objectClass = "data.frame",
      desc = "Proportion model parameters from Boudewyn et al. 2007",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6.csv"),
    expectsInput(
      objectName = "table6URL", objectClass = "character",
      desc = "URL for table 6"),
    expectsInput(
      objectName = "table7", objectClass = "data.frame",
      desc = "Caps on proportion models from Boudewyn et al. 2007.",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7.csv"),
    expectsInput(
      objectName = "table7URL", objectClass = "character",
      desc = "URL for table 7"),
    expectsInput(
      objectName = "spatialDT", objectClass = "data.table",
      desc = "the table containing one line per pixel",
      sourceURL = NA),
    expectsInput(
      objectName = "curveID", objectClass = "character",
      desc = "Vector of column names that together, uniquely define growth curve id",
      sourceURL = NA),
    expectsInput(
      objectName = "gcMeta", objectClass = "data.frame",
      desc = paste("Provides equivalent between provincial boundaries",
                   "CBM-id for provincial boundaries and CBM-spatial unit ids"),
      sourceURL =
        "https://drive.google.com/file/d/189SFlySTt0Zs6k57-PzQMuQ29LmycDmJ/view?usp=sharing"),
    expectsInput(
      objectName = "gcMetaURL", objectClass = "character",
      desc = "URL for gcMeta")
  ),
  outputObjects = bindrows(
    # createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(
      objectName = "volCurves", objectClass = "plot",
      desc = "Plot of all the growth curve provided by the user"),
    createsOutput(
      objectName = "gcMetaAllCols", objectClass = "data.frame",
      desc = "`gcMeta` as above plus ecozones"),
    createsOutput(
      objectName = "cumPoolsClean", objectClass = "data.table",
      desc = "Tonnes of carbon/ha both cumnulative and increments,
      for each growth curve id (in this data.table id and gcids are
      the same), by age and ecozone"),
    createsOutput(
      objectName = "growth_increments", objectClass = "matrix",
      desc = "Carbon increment matrix by age for each gcids")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.CBM_vol2biomass_SK <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "CBM_vol2biomass_SK", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "CBM_vol2biomass_SK", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "CBM_vol2biomass", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
      "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'",
      sep = ""
    ))
  )
  return(invisible(sim))
}

## event functions

Init <- function(sim) {
  # user provides userGcM3: incoming cumulative m3/ha
  # plot
  # Test for steps of 1 in the yield curves
  ##TODO have to make this more generic. Right now names of columns are fixed.

  ageJumps <- sim$userGcM3[, list(jumps = unique(diff(as.numeric(Age)))), by = "gcids"]
  idsWithJumpGT1 <- ageJumps[jumps > 1]$gcids
  if (length(idsWithJumpGT1)) {
    missingAboveMin <- sim$userGcM3[, approx(Age, MerchVolume, xout = setdiff(seq(0, max(Age)), Age)),
                                    by = "gcids"]
    setnames(missingAboveMin, c("x", "y"), c("Age", "MerchVolume"))
    missingAboveMin <- na.omit(missingAboveMin)
    sim$userGcM3 <- rbindlist(list(sim$userGcM3, missingAboveMin))
    setorderv(sim$userGcM3, c("gcids", "Age"))

    # Assertion
    ageJumps <- sim$userGcM3[, list(jumps = unique(diff(as.numeric(Age)))), by = "gcids"]
    idsWithJumpGT1 <- ageJumps[jumps > 1]$gcids
    if (length(idsWithJumpGT1) > 0)
      stop("There are still yield curves that are not annually resolved")
  }

  sim$volCurves <- ggplot(data = sim$userGcM3, aes(x = Age, y = MerchVolume, group = gcids, colour = factor(gcids))) +
    geom_line() + theme_bw() ## TODO: change to Plots()
  message("User: please look at the curve you provided via sim$volCurves")
  ## not all curves provided are used in the simulation - and ***FOR NOW*** each
  ## pixels only gets assigned one growth curve (no transition, no change in
  ## productivity).

  userGcM3 <- sim$userGcM3
    #}
  spu <- unique(sim$spatialDT$spatial_unit_id)
  eco <- unique(sim$spatialDT$ecozones)

  thisAdmin <- sim$cbmAdmin[sim$cbmAdmin$SpatialUnitID %in% spu & sim$cbmAdmin$EcoBoundaryID %in% eco, ]


  # START reducing Biomass model parameter tables -----------------------------------------------
  # not all ecozones are in tables 3-7. There may be some mismatch here.
  # these are the ecozones in the tables
  # id               name
  # 4       Taiga Plains
  # 5  Taiga Shield West
  # 6 Boreal Shield West
  # 7  Atlantic Maritime
  # 9      Boreal Plains
  # 10  Subhumid Prairies
  # 12  Boreal Cordillera
  # 13   Pacific Maritime
  # 14 Montane Cordillera
  # these are the ones that are not.
  # id               name
  # 8   Mixedwood Plains  - 7  Atlantic Maritime
  # 11   Taiga Cordillera - 4 taiga plains
  # 15      Hudson Plains - 6 Boreal Shield West
  # 16  Taiga Shield East - 5  Taiga Shield West
  # 17 Boreal Shield East - 6 Boreal Shield West
  # 18  Semiarid Prairies - 10  Subhumid Prairies

  ecoNotInT <- c(8, 11, 15, 16, 17, 18)
  if (any(eco %in% ecoNotInT)) {
    EcoBoundaryID <- c(7, 4, 6, 5, 6, 10)
    ecoReplace <- data.table(ecoNotInT, EcoBoundaryID)
    thisAdmin <- merge(ecoReplace, thisAdmin, by.x = "ecoNotInT", by.y = "EcoBoundaryID")
  }

  if (any(eco %in% ecoNotInT)) {
    stable3 <- as.data.table(sim$table3[sim$table3$juris_id %in% thisAdmin$abreviation &
                                          sim$table3$ecozone %in% thisAdmin$EcoBoundaryID, ])
    stable4 <- as.data.table(sim$table4[sim$table4$juris_id %in% thisAdmin$abreviation &
                                          sim$table4$ecozone %in% thisAdmin$EcoBoundaryID, ])
  } else {
    stable3 <- as.data.table(sim$table3[sim$table3$juris_id %in% thisAdmin$abreviation &
                                          sim$table3$ecozone %in% eco, ])
    stable4 <- as.data.table(sim$table4[sim$table4$juris_id %in% thisAdmin$abreviation &
                                          sim$table4$ecozone %in% eco, ])
  }

  abreviation <- c("PE", "QC", "ON", "MB", "SK", "YK", "NU", "NS")
  ## DANGER HARD CODED: if NFIS changes table 5, this will no longer be valid
  # juris_id: there are only 5/13 possible
  # these are the provinces available: AB BC NB NF NT
  # for the non match these would be the equivalent
  # "PE" - NB
  # "QC" - NB
  # "ON" - NB
  # "MB" - AB
  # "SK" - AB
  # "YK" - NT
  # "NU" - NT
  # "NS" - NB
  if (any(thisAdmin$abreviation %in% abreviation)) {
    t5abreviation <- c("NB", "NL", "NL", "AB", "AB", "NT", "NT", "NB")
    abreviationReplace <- data.table(abreviation, t5abreviation)
    # replace the abbreviations and select
    thisAdmin5 <- merge(abreviationReplace, thisAdmin)
    thisAdmin5[, c("abreviation", "t5abreviation") := list(t5abreviation, NULL)]
    stable5.2 <- as.data.table(sim$table5[sim$table5$juris_id %in% thisAdmin5$abreviation, ])
    stable5 <- stable5.2[ecozone %in% thisAdmin$EcoBoundaryID, ]
  } else {
    stable5.2 <- as.data.table(sim$table5[sim$table5$juris_id %in% thisAdmin$abreviation, ])
    stable5 <- stable5.2[ecozone %in% thisAdmin$EcoBoundaryID, ]
  }
  # This second "if-statement" is to catch is the "no-ecozone" match
  ### THIS NEEDS TO BE TESTED
  if (nrow(stable5.2) > 0) {
    stable5 <- stable5.2[ecozone %in% thisAdmin$EcoBoundaryID, ]
  } else {
    stop(
      "There are no matches found for the parameters needed to execute the Boudewyn models.",
      "Please manually find matches for table 5."
    )
  }

  if (nrow(stable5) < 1) {
    stop("There is a problem finding a parameter match in table 5.")
  }

  if (any(eco %in% ecoNotInT)) {
    stable6 <- as.data.table(sim$table6[sim$table6$juris_id %in% thisAdmin$abreviation &
                                          sim$table6$ecozone %in% thisAdmin$EcoBoundaryID, ])
    stable7 <- as.data.table(sim$table7[sim$table7$juris_id %in% thisAdmin$abreviation &
                                          sim$table7$ecozone %in% thisAdmin$EcoBoundaryID, ])
  } else {
    stable6 <- as.data.table(sim$table6[sim$table6$juris_id %in% thisAdmin$abreviation &
                                          sim$table6$ecozone %in% eco, ])
    stable7 <- as.data.table(sim$table7[sim$table7$juris_id %in% thisAdmin$abreviation &
                                          sim$table7$ecozone %in% eco, ])
  }

  # END reducing Biomass model parameter tables -----------------------------------------------

  # Read-in user provided meta data for growth curves. This could be a complete
  # data frame with the same columns as gcMetaEg.csv OR is could be only curve
  # id and species.
  ## Check that all required columns are available:
  ## "gcids" "species" "canfi_species" "genus" "forest_type_id"
  gcMeta <- sim$gcMeta

  if (!all(c(sim$curveID, "species") %in% names(gcMeta))) stop(
    "gcMeta is missing column(s): ",
    paste(shQuote(setdiff(c(sim$curveID, "species"), names(gcMeta))), collapse = ", "))

  if (!all(c("canfi_species", "genus", "forest_type_id") %in% names(gcMeta))) {

    gcMeta[, species_lower := trimws(tolower(species))]

    # this builds the LandRSpecies table that has all the possible options for canfi_species, genus (4 letter code), species (3 letter code)
    landRSpecies <- LandR::sppEquivalencies_CA[, .(
      canfi_code     = CanfiCode,
      name           = tolower(EN_generic_full),
      forest_type_id = sapply(Broadleaf, ifelse, "1", "3"),
      NFI
    )] |>
      tidyr::extract(NFI, into = c("genus", "species"), "(.*)_([^_]+)$") |>
      data.table::as.data.table()

    # check if all the species are in the canfi_species table
    if (!all(gcMeta$species_lower %in% landRSpecies$name)) stop(
      "gcMeta specie(s) not found in LandR::sppEquivalencies_CA table: ",
      paste(shQuote(
        unique(subset(gcMeta, !species_lower %in% landRSpecies$name)$species)
      ), collapse = ", "))

    gcMeta <- merge(
      gcMeta,
      landRSpecies[, .(species_lower = name, canfi_code, genus, forest_type_id)],
      by = "species_lower")[, species_lower := NULL]
    data.table::setkey(gcMeta, gcids)
  }

  setkey(gcMeta, gcids)
  if (!unique(unique(userGcM3$gcids) == unique(gcMeta$gcids))) {
    stop("There is a missmatch in the growth curves of the userGcM3 and the gcMeta")
  }

  # assuming gcMeta has now 5 columns, it needs 2 more: spatial_unit_id and ecozone. This
  # will be used in the convertM3biom() fnct to link to the right ecozone
  # and it only needs the gc we are using in this sim.
  gcThisSim <- unique(sim$spatialDT[,.(gcids, spatial_unit_id, ecozones)])
  setkey(gcThisSim, gcids)
  setkey(gcMeta, gcids)
  gcMeta <- merge(gcMeta, gcThisSim)

  sim$gcMetaAllCols <- gcMeta

  # START processing curves from m3/ha to tonnes of C/ha then to annual increments
  # per above ground biomass pools -------------------------------------------

  # 1. Calculate the translation (result is cumPools or "cumulative AGcarbon pools")

  # Matching is 1st on species, then on gcids which gives us location (admin,
  # spatial unit and ecozone)
  fullSpecies <- unique(gcMeta$species) ## RIA: change this to the canfi_sps or match??
  ####cumPools <- NULL

  cumPools <- Cache(cumPoolsCreate, fullSpecies, gcMeta, userGcM3,
                    stable3, stable4, stable5, stable6, stable7, thisAdmin)
  cbmAboveGroundPoolColNames <- "totMerch|fol|other"
  colNames <- grep(cbmAboveGroundPoolColNames, colnames(cumPools), value = TRUE)

  # 2. MAKE SURE THE PROVIDED CURVES ARE ANNUAL
  ### if not, we need to extrapolate to make them annual
  minAgeId <- cumPools[,.(minAge = max(0, min(age) - 1)), by = "gcids"]
  fill0s <- minAgeId[,.(age = seq(from = 0, to = minAge, by = 1)), by = "gcids"]
  # might not need this
  length0s <- fill0s[,.(toMinAge = length(age)), by = "gcids"]
  # these are going to be 0s
  carbonVars <- data.table(gcids = unique(fill0s$gcids),
                           totMerch = 0,
                           fol = 0,
                           other = 0 )

  fiveOf7cols <- fill0s[carbonVars, on = "gcids"]

  otherVars <- cumPools[,.(id = unique(id), ecozone = unique(ecozone)), by = "gcids"]
  add0s <- fiveOf7cols[otherVars, on = "gcids"]
  cumPoolsRaw <- rbindlist(list(cumPools,add0s), use.names = TRUE)
  set(cumPoolsRaw, NULL, "age", as.numeric(cumPoolsRaw$age))
  setorderv(cumPoolsRaw, c("gcids", "age"))

  # 3. Plot the curves that are directly out of the Boudewyn-translation
  # Usually, these need to be, at a minimum, smoothed out.
  if (!is.null(P(sim)$outputFigurePath) || !is.na(P(sim)$outputFigurePath)){
    figPath <- file.path(outputPath(sim), "CBM_vol2biomass_figures")
    dir.create(figPath, recursive = TRUE, showWarnings = FALSE)
  }else{
    figPath <- P(sim)$outputFigurePath
    if (!file.exists(figPath)) stop("Output figure path not found: ", figPath)
  }

  # plotting and save the plots of the raw-translation in the sim$ don't really
  # need this b/c the next use of m3ToBiomPlots fnct plots all 6 curves, 3
  # raw-translation and 3-smoothed curves resulting from the Chapman-Richards
  # parameter finding in the cumPoolsSmooth fnct. Leaving these lines here as
  # exploration tools.
  # if (!is.na(P(sim)$.plotInitialTime))
  sim$plotsRawCumulativeBiomass <- m3ToBiomPlots( inc = cumPoolsRaw,
                                         path = figPath,
                                         filenameBase = "rawCumBiomass_")

  # Fixing of non-smooth curves
  ## SK is a great example of poor performance of the Boudewyn et al 2007
  ## models. The "translation" does not work well with white birch (probably
  ## because there was not enough data in SK in the model-building data). So,
  ## the resulting curves are for fol and other are nonsensical. This can be
  ## seen by visually inspecting the curves going into the translations (run
  ## m3ToBiomPlots commented above). Here, the user, decided that after all the
  ## catches in place in the cumSmoothPools failed, a hard fix was needed. The
  ## fol and other columns in gcids 37 and 58, will be replace by the fol and
  ## other of gcids 55.
##TODO replace this hardcoding
  birchGcIds <- c("37", "58")
  birchColsChg <- c("fol", "other")
  ##TODO this (which curve to replace the wonky ones with) will have to be
  ##decided by the user after they look at all the curves.
  if(any(cumPoolsRaw$gcids == 37 | cumPoolsRaw$gcids == 58)) {
  if (any(cumPoolsRaw$gcids == 55)) {
    cumPoolsRaw[gcids %in% birchGcIds, fol := rep(cumPoolsRaw[gcids == 55, fol],length(birchGcIds))]
    cumPoolsRaw[gcids %in% birchGcIds, other := rep(cumPoolsRaw[gcids == 55, other],length(birchGcIds))]
  }else{
    meta55 <- sim$gcMeta[gcids == 55,]
    setnames(meta55, "gcids", "gcids")
    meta55$spatial_unit_id <- 28
    meta55$ecozones <- 9
    gc55 <- cumPoolsCreate(meta55$species, meta55, userGcM3[gcids == 55,],
                               stable3, stable4, stable5, stable6, stable7, thisAdmin)
    ##adding the age 0 and 0 growth
    gc550s <- data.frame(id = 55, age = 0, totMerch = 0, fol = 0, other = 0, ecozone = 9, gcids = 55)
    gc55raw <- rbind(gc55, gc550s)
    setorderv(gc55raw, c("gcids", "age"))
    cumPoolsRaw[gcids %in% birchGcIds,fol := gc55raw[, fol]]
    cumPoolsRaw[gcids %in% birchGcIds,other := gc55raw[, other]]
  }
  }

  cumPoolsClean <- cumPoolsSmooth(cumPoolsRaw
                                  ) |> Cache()
  #Note: this will produce a warning if one of the curve smoothing efforts doesn't converge


  # a[, totMerch := totMerchNew]
  #if (!is.na(P(sim)$.plotInitialTime)) {
    figs <- m3ToBiomPlots(inc = cumPoolsClean,
                  path = figPath,
                  filenameBase = "cumPools_smoothed_postChapmanRichards"
                  ) |> Cache()

  ## keeping the new curves - at this point they are still cumulative
  set(cumPoolsClean, NULL, colNames, NULL)
  colNamesNew <- grep(cbmAboveGroundPoolColNames, colnames(cumPoolsClean), value = TRUE)
  setnames(cumPoolsClean, old = colNamesNew, new = colNames)

  # 4. Calculating Increments
  incCols <- c("incMerch", "incFol", "incOther")
  cumPoolsClean[, (incCols) := lapply(.SD, function(x) c(NA, diff(x))), .SDcols = colNames,
                by = eval("gcids")]
  colsToUse33 <- c("age", "gcids", incCols)
#  if (!is.na(P(sim)$.plotInitialTime)) {
    rawIncPlots <- m3ToBiomPlots(inc = cumPoolsClean[, ..colsToUse33],
                         path = figPath,
                         title = "Smoothed increments merch fol other by gc id",
                         filenameBase = "Increments") |> Cache()
#  }
  message(crayon::red("User: please inspect figures of the raw and smoothed translation of your growth curves in: ",
                      figPath))
  sim$cumPoolsClean <- cumPoolsClean
  colsToUseForestType <- c("forest_type_id", "gcids")
  forestType <- unique(gcMeta[, ..colsToUseForestType])
  #       #FYI:
  #       # cbmTables$forest_type
  #       # id           name
  #       # 1  1       Softwood
  #       # 2  2      Mixedwood
  #       # 3  3       Hardwood
  #       # 4  9 Not Applicable

  setkeyv(forestType, "gcids")
  cumPoolsClean <- merge(cumPoolsClean, forestType, by = "gcids",
                                     all.x = TRUE, all.y = FALSE)

  ## libcbm functions are expecting a full time step increments of carbon (NOT
  ## halved). For the default CBM3-like operations that cbm_exn (libcbm) uses
  ## the increments you provide are halved internally by this code:
  ## https://github.com/cat-cfs/libcbm_py/blob/main/libcbm/model/cbm_exn/cbm_exn_annual_process_dynamics.py#L22

  outCols <- c("id", "ecozone", "totMerch", "fol", "other")
  cumPoolsClean[, (outCols) := NULL]
  keepCols <- c("gcids", "age", "merch_inc", "foliage_inc", "other_inc", "forest_type_id")
  incCols <- c("merch_inc", "foliage_inc", "other_inc")
  setnames(cumPoolsClean,names(cumPoolsClean),
           keepCols)
  increments <- cumPoolsClean[, (incCols) := list(
    merch_inc, foliage_inc, other_inc
  )]
  setorderv(increments, c("gcids", "age"))

  # Assertions
  if (isTRUE(P(sim)$doAssertions)) {
    # All should have same min age
    if (length(unique(increments[, min(age), by = "forest_type_id"]$V1)) != 1)
      stop("All ages should start at the same age for each curveID")
    if (length(unique(increments[, max(age), by = "forest_type_id"]$V1)) != 1)
      stop("All ages should end at the same age for each curveID")
  }
  ## replace increments that are NA with 0s

  increments[is.na(increments), ] <- 0
  sim$growth_increments <- increments


  # END process growth curves -------------------------------------------------------------------------------
  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  # Plot(sim$object)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  # cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  # dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  # message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("gcids", sim)) {
    ## this is where the pixelGroups and their spu eco etc.
    message("No spatial information was provided for the growth curves.
            The default values (SK simulations) will be used to limit the number of growth curves used.")
    sim$gcids <- gcidsSK
  }

  if (!suppliedElsewhere("ecozones", sim)) {
    message("No spatial information was provided for the growth curves.
            The default values (SK simulations) will be used to determine which ecozones these curves are in.")
    sim$ecozones <- ecozonesSK
  }
  if (!suppliedElsewhere("spatialUnits", sim)) {
    message("No spatial information was provided for the growth curves.
            The default values (SK simulations) will be used to determine which CBM-spatial units these curves are in.")
    sim$spatialUnits <- spatialUnitsSK
  }

  # Growth and yield
  ## TODO add a data manipulation to adjust if the m3 are not given on a yearly basis.
  if (suppliedElsewhere("userGcM3", sim) | suppliedElsewhere("userGcM3URL", sim)){

    if (suppliedElsewhere("userGcM3", sim)){

      if (!inherits(sim$userGcM3, "data.table")){

        sim$userGcM3 <- tryCatch(
          data.table::as.data.table(sim$userGcM3),
          error = function(e) stop(
            "'userGcM3' could not be converted to data.table: ", e$message, call. = FALSE))
      }

    }else if (suppliedElsewhere("userGcM3URL", sim)){

      sim$userGcM3 <- prepInputs(url = sim$userGcM3URL,
                                 destinationPath = inputPath(sim),
                                 fun = "data.table::fread")
    }

    reqCols <- c("gcids", "Age", "MerchVolume")
    if (!all(reqCols %in% names(sim$userGcM3))) stop(
      "'userGcM3' must have the following columns: ",
      paste(shQuote(reqCols), collapse = ", "))

  }else{

    message("User has not supplied growth curves ('userGcM3' or 'userGcM3URL'). ",
            "Defaults for Saskatchewan will be used.")

    sim$userGcM3 <- prepInputs(url = extractURL("userGcM3"),
                               destinationPath = inputPath(sim),
                               targetFile = "userGcM3.csv",
                               fun = "data.table::fread")
    names(sim$userGcM3) <- c("gcids", "Age", "MerchVolume")

  }


  if (!suppliedElsewhere("curveID", sim)) {
    sim$curveID <- c("gcids")#, "ecozones")
  }

  ## tables from Boudewyn -- all downloaded from the NFIS site.
  ## however, NFIS changes the tables and seems to forget parameter columns at times.
  if (!suppliedElsewhere("table3", sim)) {
    if (!suppliedElsewhere("table3URL", sim)) {
      sim$table3URL <- extractURL("table3")
    }
    sim$table3 <- prepInputs(url = sim$table3URL,
                             destinationPath = inputPath(sim),
                             fun = fread)
      }

  if (!suppliedElsewhere("table4", sim)) {
    if (!suppliedElsewhere("table4URL", sim)) {
      sim$table4URL <- extractURL("table4")
    }
    sim$table4 <- prepInputs(url = sim$table4URL,
                             destinationPath = inputPath(sim),
                             fun = fread)
      }


  if (!suppliedElsewhere("table5", sim)) {
    if (!suppliedElsewhere("table5URL", sim)) {
      sim$table5URL <- extractURL("table5")
    }
    sim$table5 <- prepInputs(url = sim$table5URL,
                             destinationPath = inputPath(sim),
                             fun = fread)
      }


  if (!suppliedElsewhere("table6", sim)) {
    if (!suppliedElsewhere("table6URL", sim)) {
      sim$table6URL <- extractURL("table6")
    }
    sim$table6 <- prepInputs(url = sim$table6URL,
                             destinationPath = inputPath(sim),
                             fun = fread)
      }

  if (!suppliedElsewhere("table7", sim)) {
    if (!suppliedElsewhere("table7URL", sim)) {
      sim$table7URL <- extractURL("table7")
    }
    sim$table7 <- prepInputs(url = sim$table7URL,
                             destinationPath = inputPath(sim),
                             fun = fread)
      }


  if (!suppliedElsewhere("gcMeta", sim)) {
    if (!suppliedElsewhere("gcMetaURL", sim)) {
      sim$gcMetaURL <- extractURL("gcMeta")
    }

        sim$gcMeta <- prepInputs(url = sim$gcMetaURL,
                                 targetFile = "gcMetaEg.csv",
                                 destinationPath = inputPath(sim),
                                 fun = fread,
                                 purge = 7
                                 )
  }

  # cbmAdmin: this is needed to match species and parameters. Boudewyn et al 2007
  # abbreviation and cbm spatial units and ecoBoudnary id is provided with the
  # adminName to avoid confusion.
  if (!suppliedElsewhere("cbmAdmin", sim)) {
    if (!suppliedElsewhere("cbmAdminURL", sim)) {
      sim$cbmAdminURL <- extractURL("cbmAdmin")
    }
        sim$cbmAdmin <- prepInputs(url = sim$cbmAdminURL,
                                   targetFile = "cbmAdmin.csv",
                                   destinationPath = inputPath(sim),
                                   fun = fread)
  }


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
