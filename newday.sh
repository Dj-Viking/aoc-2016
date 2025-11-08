set -e

if [ -z $1 ]; then
	echo "what day are you starting? 1-25";
	exit 1;
fi

if [ -z $2 ]; then
	echo "what programming language are you using?";
	exit 1;
fi

# create dir for solving puzzles

DAY="day"
NUM=$1
LANG_TO_USE=$2

LANG_DIR="$LANG_TO_USE/";
FOLDER="$LANG_TO_USE/$DAY$NUM"

if ! [ -d $FOLDER ]; then
	mkdir "$LANG_TO_USE/";
	mkdir "$LANG_TO_USE/$DAY$NUM";
	pushd "$LANG_TO_USE/$DAY$NUM" >> /dev/null;
else
	pushd "$FOLDER" >> /dev/null;
fi

# setup file

if [ "$LANG_TO_USE" = "c" ]; then
	cat << EOF > main.c 
#include <stdio.h>

int main() 
{
	printf("hello day1");
	return 0;
}
EOF
fi

if [ "$LANG_TO_USE" = "ps1" ]; then
	cat << EOF > main.ps1 
param()
\$answer = \$null

write-host "\$answer"
EOF
fi

if [ "$LANG_TO_USE" = "cobol" ]; then
	cp -r ~/projects/hello-cobol/probe .;

	cp -r probe "day$NUM";
	rm -rf probe;
	cd "day$NUM";
	cp -r . ..;
	cd ..;
	rm -rf "day$NUM";
	mv math.cob "day$NUM.cob";

fi

popd >> /dev/null
