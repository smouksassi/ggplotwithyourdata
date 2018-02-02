trelliscopePageNavInput <- function(inputId,page_current,pages_total){
  goodIncrements <- c(1, 2, 5, 10, 25, 50, 100, 500, 1000, 10000)
  skips <- goodIncrements[goodIncrements < pages_total / 2]

  skip_tags <- 
    tagList(
      lapply(skips,
        function(i) tags$li(tags$a(href="javascript:",paste(i,'x',sep='')))
      )
    )

  div(class="input-group trelliscope-page-nav",
    tags$input(
      id=inputId,
      type="number",class="form-control ppp-input trelliscope-page-current",
      value="1",min="1",max=pages_total,step="1"
    ),
    div(class="input-group-btn",
      tags$button(type="button",class="btn btn-default trelliscope-page-end", '/',
        span(class="trelliscope-pages-total",pages_total)
      ),
      tags$button(type="button",class="btn btn-default trelliscope-back-button",tabindex="-1",
        icon('arrow-left')
      ),
      tags$button(type="button",class="btn btn-default trelliscope-forward-button",tabindex="-1",
        icon('arrow-right')
      ),
      tags$button(type="button",class="btn btn-default dropdown-toggle",
        `data-toggle`="dropdown", span(class="trelliscope-active-skip",'1x'), span(class="caret"),
        span(class="sr-only",'Toggle Dropdown')
      ),
      tags$ul(class="dropdown-menu",role="menu",
        style="min-width:0px; right:0; left:auto;", skip_tags
      )
    )
  )
}
