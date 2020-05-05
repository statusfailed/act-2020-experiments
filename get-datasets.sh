#!/usr/bin/env bash

mkdir -p data
rm data/*
pushd data

wget http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data
wget http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz
wget http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz
wget http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz
wget http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz

gunzip *.gz
popd
