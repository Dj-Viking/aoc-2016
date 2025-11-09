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

# pushd "$FOLDER" >> /dev/null
pushd "$FOLDER"

if [ "$LANG_TO_USE" = "c" ]; then
	echo "compiling c"; 
	gcc -Wall -pedantic -o main main.c && ./main
fi

if [ "$LANG_TO_USE" = "ps1" ]; then
	echo "running powershell"
	pwsh -noprofile -file "main.ps1"
fi

if [ "$LANG_TO_USE" = "cobol" ]; then
	echo "compiling cobol"; 

	# clean up
	if [ -d "./dist" ]; then
		if [ -z "./dist/day$NUM" ]; then
			rm "./dist/day$NUM";
		fi
	fi

	# make dist dir if not exist
	if ! [ -d "dist" ]; then
		mkdir dist;
		pushd dist;
	else
		pushd dist;
	fi

	# assumes you have gnucobol installed somewhere in your path
	#compile and run
	cobc -x "../day$NUM.cob" \
		&& echo "" \
		&& echo "running..." \
		&& echo "" \
		&& time ./day$NUM \
		&& echo "" \
		&& echo "done..." \
		&& echo "";

	popd
fi

popd >> /dev/null
