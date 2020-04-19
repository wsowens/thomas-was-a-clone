default: thomas.js

clean:
	rm -rf elm-stuff
	rm thomas.js

thomas.js:
	elm make src/Main.elm --optimize --output="thomas.js"