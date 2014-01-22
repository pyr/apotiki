apotiki: a fast debian repository generator
===========================================

apotiki generates debian repositories fast. its goal is
to be a great companion to [fpm](https://github.com/jordansissel/fpm) and
[jenkins](http://jenkins-ci.com).

apotiki operates with the following features and constraints:

* supports a single debian release
* supports a single debian component
* supports an arbitrary number of architectures which need to be preprovisionned
* requires a valid PGP private key for signing

## Building

apotiki is a haskell program and relies on both the ghc compiler and
cabal. They are probably already available in your platform of choice.

Once cabal is installed, just run:

```bash
cabal build
```

## Installing

You can either run `cabal install` locally or distribute the built
executable available in `dist/build/apotiki/apotiki`.

## Configuring

For now the configuration is a serialized haskell structure:

```haskell
ApotikiConfig {
  keyPath = "/etc/apotiki.key", -- path to a PGP private key
  architectures = ["amd64"],    -- list of supported architectures
  component = "main",           -- debian release component
  release = "precise",          -- debian release name
  label = "Apotiki",            -- release label
  origin = "Apotiki",           -- release origin
  poolDir = "/tmp/repo/pool",   -- where packages live
  distDir = "/tmp/repo/dist"    -- where release description files live
}
```
The PGP private key can you wish to use can be exported with:

```
gpg --export-secret-keys repository-key@your.domain > /etc/apotiki.key
```

The config file path can be controlled with the `APOTIKI_CONFIG` environment
variable.

## Running

apotiki only accepts a list of debian packages on the command line.

