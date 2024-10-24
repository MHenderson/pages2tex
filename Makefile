install:
	sudo apt update -qq
	sudo apt install --no-install-recommends software-properties-common dirmngr
	wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
	sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/"
	sudo apt install --no-install-recommends r-base
