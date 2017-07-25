#!/bin/bash

# To run this script: source ./setup_dev1.sh
sudo apt-get update
sudo apt-get -y install python-pip
sudo apt -y install python3-pip
sudo apt-get -y upgrade
pip3 install --upgrade pip
pip install --upgrade pip
sudo apt-get -y install ipython ipython-notebook
sudo -H pip install jupyter 	

echo "Creating virtual environment called dev1"
sudo pip3 install virtualenv
cd ~
mkdir venvs
cd ~/venv
virtualenv --system-sitse-packages dev1
source dev1/bin/activate
echo "Installing python data science modules"
sudo apt-get install sqlite3
pip3 install sklearn
pip3 install numpy
pip3 install pandas
pip3 install matplotlib
pip3 install sklearn
pip3 install matplotlib
pip3 install ggplot
pip3 install --upgrade tensorflow
echo "Completed"

