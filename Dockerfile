FROM jahkeup/ubuntu:saucy
RUN apt-get update
RUN apt-get install -y build-essential git cabal-install zlib1g-dev libbz2-dev
RUN git clone https://github.com/pyr/apotiki
RUN cabal update
RUN cd apotiki ; cabal install