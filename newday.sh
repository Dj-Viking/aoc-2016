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

if [ "$LANG" = "cobol" ]; then
	cat << EOF > main.cob
>>SOURCE FORMAT IS FREE
          IDENTIFICATION DIVISION.
          PROGRAM-ID. main.
          ENVIRONMENT DIVISION.
          DATA DIVISION.
          WORKING-STORAGE SECTION.
          01 vars-to-add.
      *>     single digit (01)
             05 a    pic 9(01) value 2.
             05 b    pic 9(01) value 2.
          PROCEDURE DIVISION.
             add a to b.
             display "Add result -> " b. *> should display 4
             stop run.
EOF
fi

popd >> /dev/null
