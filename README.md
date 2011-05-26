# Jaco

Jaco is a web framework built on top of Compojure.
Features include named routes, validation and processing of
parameters, standard crud interface (well, last one isn't complete
now)


## Usage

See
[midje tests](https://github.com/dnaumov/jaco/tree/master/test/jaco/test/core);
more docs will be available soon

## Installation

Add following to your `:dependencies` in project.clj:

	[jaco "0.0.1-SNAPSHOT"]
    
To run the tests, clone the repo and run:

	lein deps
	lein midje
