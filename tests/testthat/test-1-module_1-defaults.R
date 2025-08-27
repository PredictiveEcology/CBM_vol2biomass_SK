
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module runs with defaults", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "1-defaults"

  simInitInput <- SpaDES.project::setupProject(

    modules = "CBM_vol2biomass_SK",
    paths   = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$modulePath,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
    ),

    userGcSPU = data.frame(
      spatial_unit_id = 28,
      curveID         = 22
    )
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)

  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)

  expect_s4_class(simTest, "simList")


  ## Check outputs 'volCurves' ----

  expect_true(!is.null(simTest$volCurves))
  expect_true(inherits(simTest$volCurves, "ggplot"))


  ## Check output 'cumPoolsClean' ----

  expect_true(!is.null(simTest$cPoolsClean))
  expect_true(inherits(simTest$cPoolsClean, "data.table"))

  expect_true("28_22" %in% simTest$cPoolsClean$gcids)


  ## Check output 'gcMeta' ---

  expect_true(!is.null(simTest$gcMeta))
  expect_true(inherits(simTest$gcMeta, "data.table"))

  expect_true("28_22" %in% simTest$gcMeta$gcids)


  ## Check output 'growth_increments' ----

  expect_true(!is.null(simTest$growth_increments))
  expect_true(inherits(simTest$growth_increments, "data.table"))

  expect_true("28_22" %in% simTest$growth_increments$gcids)

})


