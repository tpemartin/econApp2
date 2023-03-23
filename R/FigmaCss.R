collectClassNames <- function(component, classNames=c()) {
  if(is.null(classNames)) classNames = c()
  classNames = c(classNames, component$classname)
  component |> obtainChildrenNames() -> childNames
  # if(length(childNames)==0) return(classNames)
  for(i in seq_along(childNames)){
    # i=1
    classNames = collectClassNames(component[[childNames[[i]]]], classNames)
  }
  return(classNames)
}
getCss <- function(component, css) {
  collectClassNames(component) -> cc
  assertthat::assert_that(
    stringr::str_detect(css[[1]],cc[[1]]),
    msg=glue::glue("Did you forget to Copy as Css from the '{cc[[1]]}' frame?")
  )
  if(!all(table(cc)==1)){
    tbcc = table(cc)
    tbcc[tbcc!=1] |> names() |> paste(collapse = " ") |>
      paste(
        "are duplicated.", sep =" ") -> msg
    warning(msg)
    stop("Duplicated class names")
  }
  for(i in seq_along(cc)){
    # cc[[i]]
    css |> stringr::str_which(glue::glue("\\/\\* {cc[[i]]} \\*\\/")) -> targetRow
    if(i==1) beginRow = targetRow
    css[[targetRow]]
    css[[targetRow]] |>
      paste0("\n.",cc[[i]]," {") -> cssNewTemp
    if(targetRow!=beginRow) paste0("}\n", cssNewTemp) -> cssNewTemp
    cssNewTemp -> css[[targetRow]]
  }

  lastRow = length(css)
  css[[lastRow]] |> paste0("\n}") -> css[[lastRow]]
  css |> clipr::write_clip()
  invisible(css)
}
getCssFromComponent = function(){
  css = clipr::read_clip()
  cssText = getCss(component = component, css = css)
  invisible(cssText)
}
populateSeparateCss <- function(targetFrame){
  translateCss = function(css){
    getCss(component = targetFrame, css = css)
  }

  targetFrame$css = function(){
    clipr::read_clip() -> clipText
    assertthat::assert_that(
      stringr::str_detect(clipText, "^https", negate = T) |> all(),
      msg = "Did you forget to do Copy as Css?"
    )
      translateCss(clipText)
  }
}
