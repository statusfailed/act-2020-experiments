# Experiments for the paper Reverse Derivative Ascent and Learnable Logic

# Fetch data

You'll need the following datasets:

- [iris](http://archive.ics.uci.edu/ml/datasets/Iris)
- [mnist](http://yann.lecun.com/exdb/mnist/)

You can download these using a script:

    ./get-datasets.sh

# Build & Run

    cabal build
    cabal run act2020-experiments iris  # run the iris experiment
    cabal run act2020-experiments mnist # run the MNIST experiment

# Dependencies

These should set up automagically thanks to `cabal.project' 

- [rda](https://github.com/statusfailed/rda)
- [mnist-data](https://github.com/statusfailed/mnist-data)
