#!/bin/bash

# [X] Copy project files and elm.json over to a secret folder
TMP_DIR=".elm-css-transpiler"
mkdir -p $TMP_DIR
cp -r src/ $TMP_DIR/src
cp elm.json $TMP_DIR/elm.json
(cd $TMP_DIR && yarn elm-review --config ../elm-review-elm-css-extract/preview/ --fix-all-without-prompt --debug --elmjson elm.json  && cat src/StubCssGenerator.elm)

# - [X] Run elm-review in auto-fix mode on the secret folder -> `StubCssGenerator.styles : String`  is populated in secret folder
# - [X] Create a `Platform.worker` sends `StubCssGenerator.styles` through a port, and write that to an Elm file that includes a Platform.worker setup (`ExtractCss.elm`)
# - [ ] Run `ExtractCss.elm` to output `styles.css`
# - [ ] Copy `styles.css` and `elm.js` to `dist/`
