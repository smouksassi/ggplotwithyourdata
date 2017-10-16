FROM rocker/shiny
MAINTAINER "Samer Mouksassi" samermouksassi@gmail.com

RUN apt-get update && \
   apt-get install -y --no-install-recommends \
   apt-utils \
   r-cran-curl \
   curl


# some CRAN dependencies
RUN r -e "install.packages(                                       \
  c('shiny', 'shinyjs', 'colourpicker', 'ggplot2', 'ggrepel', 'scales', 'DT','plotly'    \
  'tidyr', 'dplyr', 'Hmisc', 'quantreg', 'markdown', 'rlang', 'lazyeval', 'remotes'),  \
  repos='https://cran.rstudio.com/',                              \
  dependencies=c('Depends', 'Imports', 'LinkingTo'))              \
"

RUN r -e "                                          \
  remotes::install_github('sachsmc/ggkm')         ; \
  remotes::install_github('benjaminrich/table1')  ; \
"



# install shiny app
ENV SHINY_PATH /srv/shiny-server
ENV APP_NAME ggplotwithyourdata
RUN rm -rf ${SHINY_PATH}/*
RUN cd /tmp && \
  curl -sOL https://github.com/smouksassi/${APP_NAME}/archive/master.zip && \
  unzip master.zip
RUN cp -r /tmp/${APP_NAME}-master/shinyapp/* ${SHINY_PATH}
RUN rm -rf /tmp/*



# run the app
# CMD ["bash"] ### for debugging
CMD ["/usr/bin/shiny-server.sh"]
