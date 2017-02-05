# Goodstein sequence generator

This is a simple demo of [Goodstein sequences](https://en.wikipedia.org/wiki/Goodstein%27s_theorem#Goodstein_sequences) in the Elm language. It's not meant to be high-performance or anything, just a proof of concept.

Most of the interesting code is in `src/HeredInt.elm`, with `Main.elm` controlling the website itself.

## Run locally

* [Get Elm](https://guide.elm-lang.org/install.html)
* Clone repo
* `elm package install`
* `elm repl`

Tests:
* `npm install -g elm-test` (`npm` not found? [Get Node](https://nodejs.org/en/))
* `elm test`
