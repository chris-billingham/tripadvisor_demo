FROM rocker/rstudio

MAINTAINER "Chris Billingham" chris.billingham@gmail.com

# from the tensorflow dockerfile: 
# https://github.com/tensorflow/tensorflow/blob/master/tensorflow/tools/docker/Dockerfile

# Pick up some TF dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
        build-essential \
        curl \
        default-jdk \
        libcairo2-dev \
        libfreetype6-dev \
        libzmq3-dev \
        libssh2-1-dev \
        libxml2-dev \
        libbz2-dev \
        libpcre3-dev \
        libmariadbclient-dev \
        libmariadb-client-lgpl-dev \
        libpq-dev \
        liblzma-dev \
        pkg-config \
        python \
        python-dev \
        rsync \
        software-properties-common \
        unzip \
        && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN curl -O https://bootstrap.pypa.io/get-pip.py && \
    python get-pip.py && \
    rm get-pip.py

RUN pip --no-cache-dir install \
        ipykernel \
        jupyter \
        matplotlib \
        numpy \
        scipy \
        virtualenv \
        && \
    python -m ipykernel.kernelspec

RUN pip --no-cache-dir install \
  --upgrade https://storage.googleapis.com/tensorflow/linux/cpu/tensorflow-0.11.0rc0-cp27-none-linux_x86_64.whl

RUN install2.r --error \
    --deps TRUE \
    tidyverse \
    dplyr \
    ggplot2 \
    devtools \
    formatR \
    remotes \
    selectr \
    caTools \
    keras \
    rJava
    
RUN R -e "devtools::install_github('rstudio/tensorflow')"

CMD ["/init"]