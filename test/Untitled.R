
test.Figma = function(){
  fig = Figma()
  fig$getDiv()
  fig$.nodeInfo |> View()
  info = fig$.nodeInfo

    i=1

    parentList = list()
    parentElement = info$document
    parentList$childrenElements=parentElement$children

    populateParentList <- function(parentElement, parentList) {
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
    }

    parentList |> View()


}

infoList_i = infoList_i |>
  appendChildElements(elementChildren)
