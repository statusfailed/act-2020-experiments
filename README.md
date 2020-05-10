# Experiments for Reverse Derivative Ascent

    cabal build
    cabal run act2020-experiments iris  # run the iris experiment
    cabal run act2020-experiments mnist # run the MNIST experiment

# Data

The following script will fetch datasets for you

    ./get-datasets.sh

Specifically, it downloads these:

- [iris](http://archive.ics.uci.edu/ml/datasets/Iris)
- [mnist](http://yann.lecun.com/exdb/mnist/)

# Dependencies

These should set up automagically thanks to `cabal.project' 

- [rda](https://github.com/statusfailed/rda)
- [mnist-data](https://github.com/statusfailed/mnist-data)
