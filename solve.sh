set -e

if [ -z $1 ]; then
	echo "what day are you solving? 1-25";
	exit 1;
fi

if [ -z $2 ]; then
	echo "what language are you using?";
	exit 1;
fi

DAY="day"
NUM=$1
LANG_TO_USE=$2

FOLDER="$LANG_TO_USE/$DAY$NUM"

pushd "$FOLDER" >> /dev/null

if [ "$LANG_TO_USE" = "c" ]; then
	echo "compiling c"; 
	gcc -Wall -pedantic -o main main.c && ./main
fi

if [ "$LANG_TO_USE" = "cobol" ]; then
	echo "compiling cobol"; 
	# assumes you have gnucobol installed somewhere in your path
	# compile and run
	cobc -x "./day$NUM.cob" \
		&& echo "" \
		&& echo "running..." \
		&& echo "" \
		&& "./day$NUM" \
		&& echo "" \
		&& echo "done..." \
		&& echo "";
fi

popd >> /dev/null
