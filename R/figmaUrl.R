#' Generate a Figma div generator
#'
#' @return page in the global environment
#' @export
#'
#' @examples none
Figma = function(){
  fig = new.env()
  getNodeInfoFromFigmaUrlInClipboard() -> info
  info$document$children |>
    purrr::map(~{.x$name}) -> pageNames
  info$document$children |> setNames(pageNames)-> pages


  page = list()
  for(i in seq_along(pageNames)){
    page_i = pageNames[[i]]
    page[[page_i]] = list(
      frame = list()
    )
    # get page_i's frames
    pages[[i]]$children |> purrr::map_chr(~{.x$name}) -> availableFrames
    if(length(availableFrames)==0) next
    for(j in seq_along(availableFrames)){
      targetFrameName = availableFrames[[j]]
      targetPage = pages[[page_i]]
      targetPage$children |> purrr::keep(~{.x$name==targetFrameName}) -> targetFrame
      targetFrame <- targetFrame[[1]]
      page[[page_i]]$frame[[targetFrameName]] = buildTargetFrame(targetFrameName, targetFrame)

      page[[page_i]]$frame[[targetFrameName]] |>
        poplutateDivCss()
    }
  }
    .GlobalEnv$page <- page

  fig$getDiv = function(){
    getNodeInfoFromFigmaUrlInClipboard() -> fig$.nodeInfo
  }
  return(fig)
}

Figma2 = function(){
  fig = new.env()
  getNodeInfoFromFigmaUrlInClipboard() -> info
  info$document$children |>
    purrr::map(~{.x$name}) -> pageNames
  info$document$children |> setNames(pageNames)-> pages


  page = list()
  for(i in seq_along(pageNames)){
    page_i = pageNames[[i]]
    page[[page_i]] = list(
      frame = list()
    )
    pages[[i]]$children |> purrr::map_chr(~{.x$name}) -> targetFrame
    if(length(targetFrame)==0) next
    page[[page_i]]$frame[[targetFrame]] <- new.env()
    page[[page_i]]$frame[[targetFrame]]$classname = targetFrame
    # Each page only has one child i.e. targetFrame
    page[[page_i]]$frame[[targetFrame]]$childrenElements =
      pages[[page_i]]$children[[1]]$children

    # populate targetFrameEnv
    # debug(populateFrameEnv)
    page[[page_i]]$frame[[targetFrame]] |>
      populateFrameEnv()

    if(length(targetFrame)!=0){
      page[[page_i]]$frame[[targetFrame]] |> populateSeparateDiv()
      page[[page_i]]$frame[[targetFrame]] |> refineEnvDiv()
      page[[page_i]]$frame[[targetFrame]] |> populateSeparateCss()
    }
  }
  .GlobalEnv$page <- page

  fig$getDiv = function(){
    getNodeInfoFromFigmaUrlInClipboard() -> fig$.nodeInfo
  }
  return(fig)
}
### helpers
poplutateDivCss = function(targetFrameEnv){

    targetFrameEnv |> populateSeparateDiv()
    targetFrameEnv |> refineEnvDiv()
    targetFrameEnv |> populateSeparateCss()

}

buildTargetFrame <- function(targetFrameName, targetFrame) {
    targetFrameEnv <- new.env()
    targetFrameEnv$classname = targetFrameName
    # Each page only has one child i.e. targetFrame
    targetFrameEnv$childrenElements =
      targetFrame$children

    # populate targetFrameEnv
    # debug(populateFrameEnv)
    targetFrameEnv |>
      populateFrameEnv()

    # if(length(targetFrame)!=0){
    targetFrameEnv |> populateSeparateDiv()
    targetFrameEnv |> refineEnvDiv()
    targetFrameEnv |> populateSeparateCss()
    # }
    return(targetFrameEnv)
  }
getNodeInfoFromFigmaUrlInClipboard = function(){
  clipr::read_clip() -> figmaUrl0
  nodeInfo = getNodeInfoFromFigmaUrl(figmaUrl0)

}
getNodeInfoFromFigmaUrl <- function(figmaUrl="https://www.figma.com/file/9GQBb1dc5aYFqWj0w06jMx/Taitung-Tourism?node-id=16%3A39&t=pextNDclqsiGEIOX-0") {
  figmaAccessToken=Sys.getenv("figmaAccessToken")
  assertthat::assert_that(
    figmaAccessToken !="",
    msg="Sys.getenv(\"figmaAccessToken\") returned \"\". Please set up via Sys.setenv(figmaAccessToken='your AccessToken')"
  )
  keyPattern <- "(?<=(https://www.figma.com/file/))[^/]+"
  key = stringr::str_extract(
    figmaUrl, keyPattern
  )
  filePattern =
    paste0(
      glue::glue("(?<=(https://www.figma.com/file/{key}/))"),
      "[^/?]+")
  .file = stringr::str_extract(
    figmaUrl, filePattern
  )
  queryPattern =
    paste0(
      glue::glue("(?<=(https://www.figma.com/file/{key}/{.file}\\?))"),
      "[^?]+"
    )
  query= stringr::str_extract(
    figmaUrl, queryPattern
  )
  query |>
    stringr::str_split("&") |>
    unlist() |>
    stringr::str_split("=") -> querySplit
  purrr::map(querySplit, ~{.x[[1]]}) |> unlist() -> queryParams
  purrr::map(querySplit, ~{.x[[2]]}) -> queryValues
  info = list(
    key=key, file=.file, query=setNames(queryValues, queryParams)
  )


  .endpoint = glue::glue("https://api.figma.com/v1/files/{info$key}") |>
    paste0("?ids=",info$query$`node-id`)
  .endpoint
  httr::GET(
    url=.endpoint,
    httr::add_headers(
      `X-Figma-Token`=figmaAccessToken
    )
  ) -> request
  httr::content(request) -> nodeInfo

  return(nodeInfo)
}
populateChildElementsForInfoList_i0 <- function(infoList_i) {

  if(length(infoList_i$childrenElements)==0) return()

  infoListEnv = list(
    childElements = infoList_i$childrenElements
  )
  infoList_i$childrenElements -> childrenElements
  childrenElements |>
    purrr::map_chr(~{.x$name}) -> majorFrameNames

  elementChildrenList=vector("list", length(childrenElements)) |>
    setNames(majorFrameNames)

  childrenDiv = list()
  for(i in seq_along(majorFrameNames)){
    childrenElement_i = childrenElements[[i]]
    infoListEnv[[childrenElements[[i]]$name]]=list(
      childrenElements = childrenElement_i$children
    )
    infoListEnv[[childrenElements[[i]]$name]]$div = function(...) htmltools::div(
      class=majorFrameNames[[i]],...
    )
    childrenDiv = append(
      childrenDiv, list(infoListEnv[[childrenElements[[i]]$name]]$div())
    )
   }

  infoListEnv$div = function(...) {
    htmltools::div(
      ...,
      childrenDiv
    )
  }

  return(infoListEnv)
}
populateChildElementsForInfoList_i <- function(infoList_i) {

  if(length(infoList_i$childrenElements)==0) return()

  infoListEnv = list(
    childElements = infoList_i$childrenElements
  )
  infoList_i$childrenElements -> childrenElements
  childrenElements |>
    purrr::map_chr(~{.x$name}) -> majorFrameNames

  elementChildrenList=vector("list", length(childrenElements)) |>
    setNames(majorFrameNames)

  childrenDiv = list()
  for(i in seq_along(majorFrameNames)){
    childrenElement_i = childrenElements[[i]]
    infoList_i[[majorFrameNames[[i]]]] = new.env()
    infoList_i[[majorFrameNames[[i]]]]$childrenElements =
      childrenElement_i$children
    infoList_i[[majorFrameNames[[i]]]]$classname = majorFrameNames[[i]]
    infoList_i[[majorFrameNames[[i]]]]$div = function(...) {
      htmltools::div(
        class=infoList_i[[majorFrameNames[[i]]]]$classname,
        ...
      )
    }

    childrenDiv = append(
      childrenDiv, list(infoListEnv[[childrenElements[[i]]$name]]$div())
    )
  }

  infoListEnv$div = function(...) {
    htmltools::div(
      ...,
      childrenDiv
    )
  }

  return(infoListEnv)
}

populateParentList0 <- function(parentList) {
  parentChildrenNames = purrr::map_chr(
    parentList$childrenElements, ~{.x$name}
  )

  for(i in seq_along(parentChildrenNames)){
    child_i = parentList$childrenElements[[i]]
    # if(length(child_i)==0) next
    child_i_env = new.env()
    child_i_env$childrenElements = child_i$children

    child_i_list = list(
      childrenElements = child_i$children
    )
    if(length(child_i$children)!=0){
      grandChildNames = child_i_list$childElements |>
        purrr::map_chr(~{.x$name})

      child_i_list |>
        populateChildElementsForInfoList_i() ->
        child_i_list
      child_i_div = child_i_list$div
      child_i_list$div = function(...) {
        child_i_div(class=parentChildrenNames[[i]])
      }
      parentList[[parentChildrenNames[[i]]]] = child_i_list
    } else {
      parentList[[parentChildrenNames[[i]]]]$div = function(...){
        htmltools::div(class=parentChildrenNames[[i]])
      }
    }

  }

  return(parentList)
}
populateParentList <- function(parentList) {
  parentChildrenNames = purrr::map_chr(
    parentList$childrenElements, ~{.x$name}
  )


  length(parentList)
  envName = parentList$childrenElements[[i]]$name
  parentList[[envName]] = new.env()
  parentList[[envName]]$classname = envName
  parentList[[envName]]$childrenElements =
  debug(populateChildElementsForInfoList_i)
  parentList |>
    populateChildElementsForInfoList_i() -> child_i_list
  parentList$elements |> class()

  for(i in seq_along(parentChildrenNames)){
    child_i = parentList$childrenElements[[i]]
    # if(length(child_i)==0) next
    child_i_env = new.env()
    child_i_env$childrenElements = child_i$children

    # child_i_list = list(
    #   childrenElements = child_i$children
    # )
    if(length(child_i$children)!=0){
      grandChildNames = child_i_env$childElements |>
        purrr::map_chr(~{.x$name})

      child_i_env |>
        populateChildElementsForInfoList_i() ->
        child_i_env
      child_i_div = child_i_env$div
      child_i_env$div = function(...) {
        child_i_div(class=parentChildrenNames[[i]])
      }
      parentList[[parentChildrenNames[[i]]]] = child_i_env
    } else {
      parentList[[parentChildrenNames[[i]]]]$div = function(...){
        htmltools::div(class=parentChildrenNames[[i]])
      }
    }

  }


  return(parentList)
}
obtainChildrenNames <- function(parentList) {
  parentList |> names() -> listNames
  listNames |>
    setdiff(
      c("childrenElements", "div", "classname", "css")
    )
}
populateFrameEnv = function(frameEnv){
  if(length(frameEnv$childrenElements)==0) return()
  for(i in seq_along(frameEnv$childrenElements)){
    # i=1
    childName = frameEnv$childrenElements[[i]]$name
    frameEnv[[childName]] = new.env()
    childFrame = frameEnv[[childName]]
    childFrame$childrenElements = frameEnv$childrenElements[[i]]$children
    childFrame$classname = childName
    populateFrameEnv(childFrame)
  }
}
refineEnvDiv <- function(tf) {
  tf |> obtainChildrenNames() -> childEnvNames
  tf$div = function(...){
    divList = list()
    for(i in seq_along(childEnvNames)){
      divList = append(divList, list(tf[[childEnvNames[[i]]]]$div()))
    }
    htmltools::div(
      ...,
      divList,
      class=tf$classname
    )
  }

  for(i in seq_along(childEnvNames)){
    tf[[childEnvNames[[i]]]] |> refineEnvDiv()
    tf[[childEnvNames[[i]]]] |> populateSeparateCss()
  }
}
populateSeparateDiv <- function(targetFrame){
  targetFrame |> obtainChildrenNames() -> childNames

  targetFrame$div = function(...){
    htmltools::div(
      ..., class=classname
    )}
  environment(targetFrame$div) <- targetFrame

  for(i in seq_along(childNames)){
    populateSeparateDiv(targetFrame[[childNames[[i]]]])
  }

}
populateEnvDiv <- function(targetFrame) {
  targetFrame |> obtainChildrenNames() -> childNames

  #divText = ifelse(targetFrame$type=="TEXT", targetFrame$characters, "")

  if(length(childNames)==0){
    targetFrame$div = function(...){
      if(type=="TEXT"){
        return(
          htmltools::div(..., class = classname, characters)
        )
      } else {
        return(
          htmltools::div(..., class = classname)
        )
      }
    }
    environment(targetFrame$div) <- targetFrame
  } else {
    for(i in seq_along(childNames)) populateEnvDiv(targetFrame[[childNames[[i]]]])
    targetFrame$div <- function(...) {
      rlang::current_env() |> rlang::env_parent() -> pe
      # browser()
      # targetFrame |> obtainChildrenNames() -> childNames
      divList <- list()
      for (i in seq_along(childNames)) {
        divList <- append(divList, list(
          pe[[childNames[[i]]]]$div()))
      }
      htmltools::div(
        ...,
        divList,
        class = classname
      )
    }
    environment(targetFrame$div) <- targetFrame
  }

}
