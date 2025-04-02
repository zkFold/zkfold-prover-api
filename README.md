# zkFold Prover API
REST API server for zkFold proof generation. Leverages the [zkFold Base](https://github.com/zkFold/zkfold-base/tree/main) building blocks.

In collaboration with [Maestro](https://github.com/maestro-org)!

<img src="media/cover.png" alt="Cover Image" width="400">

# Build
## Source
The package compiles with GHC 9.12.1 and Cabal 3.14.1.1.
```
make build
```
<!-- Docker currently does not support ghc version 9.12.1 -->
<!-- ## Docker
```
make docker-build
``` -->
# Run
## Host
```
make run
```
<!-- ## Docker
```
make docker-run
``` -->

# Example query
```
curl -X POST -H "Content-Type: application/json" -d @examples/simple-witness.json localhost:8080/prove
```

# Tests
On the way!
