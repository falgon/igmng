FROM ubuntu:20.04

ENV PYTHON_VERSION=3.9.2 \
    HOME=/home/igmng \
    TZ=Asia/Tokyo \
    DEBIAN_FRONTEND=noninteractive
ENV PYTHON_ROOT=$HOME/local/python-$PYTHON_VERSION \
    PYENV_ROOT=$HOME/.pyenv
ENV PATH=$PYTHON_ROOT/bin:$PATH
RUN apt-get -qq update && apt-get install -y -qq --no-install-recommends tzdata \
 && apt-get install -y -qq --no-install-recommends \
    git \
    make \
    build-essential \
    libssl-dev \
    zlib1g-dev \
    libbz2-dev \
    libreadline-dev \
    libsqlite3-dev \
    wget \
    curl \
    llvm \
    libncurses5-dev \
    libncursesw5-dev \
    xz-utils \
    tk-dev \
    libffi-dev \
    liblzma-dev \
    ca-certificates \
 && git clone https://github.com/pyenv/pyenv.git $PYENV_ROOT \
 && $PYENV_ROOT/plugins/python-build/install.sh \
 && /usr/local/bin/python-build -v $PYTHON_VERSION $PYTHON_ROOT \
 && rm -rf $PYENV_ROOT \
 && groupadd -g 61000 igmng \
 && useradd -g 61000 -l -m -s /bin/bash -u 61000 igmng \
 && mkdir -p /home/igmng/.igmng \
 && chown -R igmng:igmng /home/igmng
COPY --chown=igmng:igmng ./igrouter/requirements.txt /home/igmng
COPY --chown=igmng:igmng ./credentials.toml /home/igmng/.igmng
USER igmng
WORKDIR /home/igmng
RUN pip3 install -r requirements.txt
