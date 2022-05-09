# Note to self

elm-app build incorrectly generates relative paths for assets in the generated index.html. They must be
manually changed back to their correct paths 🙄
Ex: "/static/js/main.74839a20.chunk.js" should be changed to "static/js/main.74839a20.chunk.js" (without the leading / )