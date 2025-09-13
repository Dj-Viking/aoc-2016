set -e

if [ -z $1 ]; then
	echo "what day are you solving? 1-25"
	exit 1;
fi

DIR=$1
DAY=day
FOLDER="$DAY$DIR"

pushd "$FOLDER" >> /dev/null

gcc -o main main.c && ./main

popd >> /dev/null
