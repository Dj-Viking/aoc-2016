set -e
DAY=$1

if [ -z $1 ]; then
	echo "what day are you starting? 1-25";
	exit 1;
fi

DIR=day

FOLDER="$DIR$DAY"

if ! [ -d $FOLDER ]; then
	mkdir "$FOLDER";
	pushd "$FOLDER" >> /dev/null;
else
	pushd $FOLDER >> /dev/null;
fi

cat << EOF > main.c 

#include <stdio.h>

int main() 
{
	printf("hello day1");
	return 0;
}

EOF

popd >> /dev/null

cd $FOLDER
