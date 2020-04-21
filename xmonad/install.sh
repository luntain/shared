set -xe
stack install xmonad
stack install
mkdir -p $HOME/.xmonad
cp ./xmonad.desktop $HOME/.local/share/applications/
echo "YOU NEED TO make a symbolic link from .xmonad to build here"
