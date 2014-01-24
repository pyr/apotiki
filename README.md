apotiki: a fast debian repository generator
===========================================

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

## Building

Apotiki is a haskell program and relies on both the ghc compiler and
cabal. They are probably already available in your platform of choice.

Once cabal is installed, just run:

```bash
cabal install
```

## Installing

You can either run `cabal install` locally or distribute the built
executable available in `dist/build/apotiki/apotiki`.

## Configuring

For now the configuration is a serialized haskell structure:

```haskell
ApotikiConfig {
  keyPath = "/etc/apotiki.key",      -- path to a PGP private key
  architectures = ["amd64", "i386"], -- list of supported architectures
  component = "main",                -- debian release component
  release = "precise",               -- debian release name
  label = "Apotiki",                 -- release label
  origin = "Apotiki",                -- release origin
  repoDir = "/srv/repo"              -- repository location, expose via http
}
```
The PGP private key you wish to use can be exported with:

```
gpg --export-secret-keys repository-key@your.domain > /etc/apotiki.key
```

The config file path can be controlled with the `APOTIKI_CONFIG` environment
variable.

## Caveats

Error handling is suboptimal to say the least. we'll get there.
