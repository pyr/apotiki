FROM jahkeup/ubuntu:saucy
RUN apt-get update
RUN apt-get install -y build-essential git cabal-install zlib1g-dev libbz2-dev
RUN cabal update
RUN cabal install apotiki
RUN mkdir /srv/repo
ADD apotiki.conf.example /etc/apotiki.conf
ADD key.pgp /srv/repo/key.pgp
EXPOSE 8000
ENTRYPOINT [".cabal/bin/apotiki", "web"]