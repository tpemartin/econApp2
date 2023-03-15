
test.Figma = function(){
  fig = Figma()
  fig$getDiv()
  info = fig$.nodeInfo

    i=1

    parentList = list()
    parentElement = info$document
    parentList$childrenElements=parentElement$children

    parentChildrenNames = purrr::map_chr(
      parentElement$children, ~{.x$name}
    )

    for(i in seq_along(parentChildrenNames)){
      child_i = parentElement$children[[i]]
      # if(length(child_i)==0) next
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

    parentList$business$div(

    )
    parentList$elements$div()

    parentList$elements$div()

    childElements = vector("list", length(parentChildrenNames)) |>
      setNames(parentChildrenNames)

    parentList |> names()
    parentList |>
      purrr::reduce()
    infoChildren = vector("list", length(infoList_i$childrenElements)) |>
      setNames(infoChildrenNames)
    infoList_i |>
      append(infoChildren) -> infoList_i

    for(i in seq_along(infoList_i$childrenElements)){
      # print(i)
      i=3
      childNameX = infoChildrenNames[[i]]
      print(childNameX)
      len_child_i_children = length(infoList_i$childrenElements[[i]]$children)

      if(len_child_i_children==0){
        infoList_i[[childNameX]]$childrenElements = list()
      } else {
        infoList_i[[childNameX]]$childrenElements = infoList_i$childrenElements[[i]]$children
        infoList_i[[childNameX]] |>
          append(
            infoList_i[[childNameX]] |> populateChildElementsForInfoList_i()
          ) -> infoList_i[[childNameX]]
      }
    }


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
