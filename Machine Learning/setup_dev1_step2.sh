#!/bin/bash

cd ~/git/EventPrediction
mkdir 3_Data
cd 3_Data
wget http://mtg.upf.edu/static/datasets/last.fm/lastfm-dataset-1K.tar.gz
head -7500000 userid-timestamp-artid-artname-traid-traname.tsv > userid-timestamp-artid-artname-traid-traname_hlf.tsv
tar -xvzf lastfm-dataset-1K.tar.gz
cd ~/git
git clone https://github.com/GPflow/GPflow.git

