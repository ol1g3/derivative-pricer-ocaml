FROM ocaml/opam:ubuntu-22.04-ocaml-5.2

WORKDIR /app

USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
    libopenblas-dev \
    liblapacke-dev \
    zlib1g-dev \
    pkg-config \
 && rm -rf /var/lib/apt/lists/*

USER opam
COPY --chown=opam:opam . .

RUN opam install -y owl && opam exec -- dune build

CMD ["opam","exec","--","dune","exec","./deriv_pricer.exe"]