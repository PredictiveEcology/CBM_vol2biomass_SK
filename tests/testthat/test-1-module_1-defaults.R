
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module runs with defaults", {

  ## Run simInit and spades ----

  # Set project path
  projectPath <- file.path(spadesTestPaths$temp$projects, "1-defaults")
  dir.create(projectPath)
  withr::local_dir(projectPath)

  # Set up project
  simInitInput <- SpaDES.project::setupProject(

    modules = "CBM_vol2biomass",
    paths   = list(
      projectPath = projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      inputPath   = spadesTestPaths$temp$inputs,
      packagePath = spadesTestPaths$temp$packages,
      cachePath   = spadesTestPaths$temp$cache,
      outputPath  = file.path(projectPath, "outputs")
    ),

    spatialDT = data.frame(
      spatial_unit_id = 28,
      ecozones        = 9,
      gcids           = 1
    )
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)

  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)

  expect_s4_class(simTest, "simList")


  ## Check output 'volCurves' ----

  expect_true(!is.null(simTest$volCurves))
  expect_true(inherits(simTest$volCurves, "ggplot"))

  ## Check output 'cumPoolsClean' ----

  expect_true(!is.null(simTest$cPoolsClean))
  expect_true(inherits(simTest$cPoolsClean, "data.table"))


  ## Check output 'growth_increments' ----

  expect_true(!is.null(simTest$growth_increments))
  expect_true(inherits(simTest$growth_increments, "data.table"))

})


