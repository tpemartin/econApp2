
test.Figma = function(){
  fig = Figma()
  fig$getDiv()
  info = fig$.nodeInfo

    i=1

    infoList = list()
    child = info$document
    infoList[[child$name]] = list(
      childrenElements=child$children
    )

    infoList_i =  infoList$Document
    infoChildrenNames = purrr::map_chr(
      infoList_i$childrenElements, ~{.x$name}
    )

    infoChildren = vector("list", length(infoList_i$childrenElements)) |>
      setNames(infoChildrenNames)
    for(i in seq_along(infoList_i$childrenElements)){
      # print(i)
      childNameX = infoChildrenNames[[i]]
      infoChildren[[childNameX]] = infoList_i$childrenElements[[i]] |> populateChildElementsForInfoList_i()
    }

    infoList_i |>
      append(infoChildren) -> infoList_i

    infoList_i$div = function(...) {
        purrr::map(infoChildrenNames, ~{
          infoList_i[[.x]]$div()
        })

    }



    infoList_ii = populateChildElementsForInfoList_i(infoList$Document)

    debug(populateChildElementsForInfoList_i)
    infoList_iii = populateChildElementsForInfoList_i(infoList_ii$business)
    infoList_iv = populateChildElementsForInfoList_i(infoList_iii$business)

}

infoList_i = infoList_i |>
  appendChildElements(elementChildren)
