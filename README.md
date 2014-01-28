apotiki: a faster debian repository
===================================

![apotiki](http://i.imgur.com/3Jmupwb.jpg)

([image source](http://commons.wikimedia.org/wiki/File:A_view_of_the_map_repository_at_The_National_Archives.jpg))

apotiki generates debian repositories fast. its goal is
to be a great companion to [fpm](https://github.com/jordansissel/fpm) and
[jenkins](http://jenkins-ci.org).

apotiki operates with the following features and constraints:

* Supports a single debian release
* Supports a single debian component
* Supports an arbitrary number of architectures which need to be preprovisionned
* Requires a valid PGP private key for signing

## The Story

You operate a production environment and rely on software that is more recent than is
available on a standard Debian or Ubuntu distribution ? Apotiki helps you distribute
software by creating a separate debian repository which you can add to your apt sources.

Turns out there's already software available for this, such as [freight](https://github.com/rcrowley/freight),
apotiki's angle is to work very fast for the most common use case.

## Companion software

[fpm](https://github.com/jordansissel/fpm) is a great tool to build Debian packages with.
It can produce packages from directories, gems, npm or pip libraries.

[jenkins](http://jenkins-ci.org) or [travis-ci](http://travis-ci.com) can produce artifacts by running
scripts.

## Using

apotiki has two modes of operation, try not to mix the two too much:

* `apotiki insert`: pushes a list of packages, given on the command line to the repo 
* `apotiki web`: start up a web service on port 8000 to display the repository and accept new packages

Running apotiki with no arguments or `help` will tell you a bit about usage.

If you wish to submit packages to the repository with curl here is the relevant command line
assuming your package file is `package-foo.deb`

```bash
curl -X POST -F "package=@/path/to/package-foo.deb" http://repo-host:8000/repo
```

## Installing

Apotiki is a haskell program and relies on both the ghc compiler and
cabal. They are probably already available in your platform of choice.
for instance, on debian based systems, just run
`apt-get install cabal-install`.

Once cabal is installed, just run:

```bash
cabal update
cabal install apotiki
```

## Building

Alternatively, you can build apotiki with docker. Just run:

```
sudo docker build .
```

The resulting container will have the built cabal executable.


## Configuring

The configuration file format resembles is a simple column
separated format, no comment lines are allowed and all configuration
keys are expected to be downcased.

```
architectures: i386 amd64
component: main
release: precise
label: Apotiki
origin: Apotiki
repo: /tmp/repo
logfile: STDOUT
pgp-key:
  -----BEGIN PGP PRIVATE KEY BLOCK-----
  Version: GnuPG v2.0.22 (GNU/Linux)

  [base64 nonsense...]
  -----END PGP PRIVATE KEY BLOCK-----
                                                            
```

* `architectures`: list of supported architectures in your repo
* `component`: name of the release component, a single component is supported for now
* `release`: name of the debian release you wish to expose
* `label` and `origin`: Debian repository format details, see https://wiki.debian.org/RepositoryFormat#Label
* `repo`: directory where the repo will live
* `logfile`: either *STDOUT* for console logging or a path to log to
* `pgp-key`: ascii-armored export of the PGP key to sign the repo with

The PGP private key you wish to use can be exported with:

```
gpg -a --export-secret-keys repository-key@your.domain
```

The config file path can be controlled with the `APOTIKI_CONFIG` environment
variable.

## Caveats

Error handling is suboptimal to say the least. we'll get there.
