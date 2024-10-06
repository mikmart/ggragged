# ggragged (development version)

* Added a vignette showing examples of usage in broader context.
* Fixed an issue that caused the package to fail to build (with an "argument is
  missing" error message) when an older version of ggplot2 was installed.
* Added new parameter `strips` to control how strips are drawn between panels.
* Added new parameter `axes` to control how axes are drawn between panels.
* Added new parameter `align` to control how panels are positioned within rows/columns.
* Fixed an issue that caused some axes to be rendered incorrectly when using
  free scales with `coord_flip()` (#2).

# ggragged 0.1.0

* Initial release.
