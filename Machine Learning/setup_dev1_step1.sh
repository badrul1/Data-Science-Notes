#!/bin/bash

# To run this script: source ./setup_dev1.sh

## Core setup ##
sudo apt-get update
sudo apt-get -y install python-pip
sudo apt -y install python3-pip
sudo apt-get install python3-tk
sudo apt-get -y upgrade
sudo apt-get y install git
sudo apt-get install sqlite3

pip3 install --upgrade pip
pip install --upgrade pip
sudo apt-get -y install ipython ipython-notebook
sudo -H pip3 install jupyter 	
pip3 install sklearn
pip3 install numpy
pip3 install pandas
pip3 install matplotlib
pip3 install sklearn
pip3 install matplotlib
pip3 install ggplot
pip3 install --upgrade tensorflow

#echo "Creating virtual environment called dev1"
#sudo pip3 install virtualenv
#mkdir ~/venvs
#cd ~/venvs
#virtualenv --system-site-packages dev1
#source dev1/bin/activate

# Install Tensorflow
wget https://developer.nvidia.com/compute/cuda/8.0/Prod2/local_installers/cuda_8.0.61_375.26_linux-run
chmod +x cuda_8.0.61_375.26_linux-run 
sudo sh ./cuda_8.0.61_375.26_linux-run
wget https://developer.nvidia.com/compute/cuda/8.0/Prod2/patches/2/cuda_8.0.61.2_linux-run
sudo sh ./cuda_8.0.61.2_linux-run

# Install Jekyll
sudo apt-get install zlib1g-dev^C
sudo gem install nokogiri
bundle install
sudo gem install bundler
echo "Completed - Type bundle exec jekyll serve for Jekyll server"
