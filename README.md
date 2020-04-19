# Elm Platformer
Simple platformer created to test the (Elm programming language).
Currently deployed here: [ovvens.com/thomas](https://ovvens.com/thomas/)

*Thomas Was Alone* is an award-winning platformer from Mike Bithell.
All credit goes to him for this little program's stylistic influences.

# Requirements
This project requires Elm (version >= 0.19).
You can find instructions for installing Elm [here](https://guide.elm-lang.org/install/elm.html).

# Getting Started
A makefile has been supplied for convenience. To compile the project, from the top directory, run 

```sh
elm make src/Main.elm --optimize --output="thomas.js"
```

A makefile has been supplied for convenience, so you could also run `make` if you so choose.

Either of these options will produce a JavaScript file, `thomas.js`.
The `index.html` file is already written to load this file.
Once you have `thomas.js`, you can load the `index.html` in your browser of choice, or serve it using your server of choice.
