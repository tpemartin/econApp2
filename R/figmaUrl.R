

figmaUrl0 = "https://www.figma.com/file/9GQBb1dc5aYFqWj0w06jMx/Taitung-Tourism?node-id=16%3A39&t=pextNDclqsiGEIOX-0"

Figma = function(){
  fig = new.env()
  fig$getDiv = function(){
    getNodeInfoFromFigmaUrlInClipboard() -> fig$.nodeInfo
  }
  return(fig)
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
