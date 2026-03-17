FROM haskell:latest

# instalar dependencias necesarias
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl build-essential libffi-dev libgmp-dev libncurses-dev pkg-config xz-utils ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# instalar ghcup no interactivo y añadir al PATH
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh \
  && . /root/.ghcup/env \
  && ghcup install ghc recommended \
  && ghcup install cabal recommended \
  && ghcup install hls recommended

ENV PATH="/root/.ghcup/bin:${PATH}"

WORKDIR /app

COPY . . /app/

CMD ["/bin/bash"]
