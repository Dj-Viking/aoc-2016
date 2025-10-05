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

if [ "$LANG" = "c" ]; then
	gcc -Wall -pedantic -o main main.c && ./main
fi

if [ "$LANG" = "cobol" ]; then
	# assumes you have gnucobol installed somewhere in your path
	# compile and run
	cobc -x "./main.cob" \
		&& echo "" \
		&& echo "running..." \
		&& echo "" \
		&& "./main" \
		&& echo "" \
		&& echo "done..." \
		&& echo "";
fi

popd >> /dev/null
