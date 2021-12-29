FROM ubuntu:20.04 as bin
SHELL ["/bin/bash", "-c"]
ENV TZ=Asia/Tokyo \
    DEBIAN_FRONTEND=noninteractive \
    DEBCONF_NOWARNINGS=yes
RUN apt-get -qq update && apt-get install -y -qq --no-install-recommends \
        curl \
        ca-certificates \
        language-pack-ja \
        libtinfo-dev \
    && curl -sSL https://get.haskellstack.org/ | sh \
    && apt-get -y -qq autoremove \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /opt/src
ENV LANG=ja_JP.UTF-8 \
    LANGUAGE=ja_JP:ja \
    LC_ALL=ja_JP.UTF-8
COPY . /opt/src
WORKDIR /opt/src
RUN mkdir -p /root/dist \
    && stack --no-terminal --allow-different-user --local-bin-path=/root/dist install

FROM ubuntu:20.04
SHELL ["/bin/bash", "-c"]
ENV TZ=Asia/Tokyo \
    DEBIAN_FRONTEND=noninteractive \
    DEBCONF_NOWARNINGS=yes
RUN apt-get -qq update && apt-get install -y -qq --no-install-recommends \
        busybox-static \
        curl \
        ca-certificates \
        language-pack-ja \
        git \
    && apt-get -y -qq autoremove \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir /root/.igmng
ENV LANG=ja_JP.UTF-8 \
    LANGUAGE=ja_JP:ja \
    LC_ALL=ja_JP.UTF-8
COPY --from=bin /root/dist /root/bin
COPY ./containers/.env /root
COPY ./credentials.toml /root/.igmng
COPY --chown=root:root ./containers/igmng_cron/crontab /var/spool/cron/crontabs/root
WORKDIR /root/bin
ENV PATH $PATH:/root/bin
