FROM rocker/binder:4.0.0
LABEL maintainer="bmarwick"
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
## Become normal user again
USER ${NB_USER}
RUN wget https://github.com/LiYingWang/kwl-ornaments/raw/master/DESCRIPTION \
  && R -e "devtools::install_deps()" \
  && R -e "remotes::install_github('benmarwick/wordcountaddin')" 

