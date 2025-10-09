
# set -xe

set -e

if [ -z "$1" ]; then
	echo "[ERROR]: missing first arg to script 
	 please provide name of file to compile and run
	 usage: 'sh build.sh math' or 'sh build.sh math.cob'"
	exit 1;
fi

FILENAME="$1"
FILE_TO_RUN="${FILENAME%.*}"

# if input arg text didn't contain the .cob extension add it and set up the other
# script vars
if ! [ "${FILENAME##*.}" = "cob" ]; then
	FILENAME="$FILENAME.cob"
fi

# clean up
if [ -d "./dist" ]; then
	if [ -z "./dist/$FILENAME" ]; then
		rm "./dist/$FILENAME";
	fi
fi

# make dist dir if not exist
if ! [ -d "dist" ]; then
	mkdir dist;
	pushd dist;
else
	pushd dist;
fi

#compile and run
cobc -x "../$FILENAME" -o "main-$FILE_TO_RUN" \
	&& echo "" \
	&& echo "running..." \
	&& echo "" \
	&& "./main-$FILE_TO_RUN" \
	&& echo "" \
	&& echo "done..." \
	&& echo "";

popd
