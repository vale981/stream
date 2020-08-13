#!/bin/sh
echo "normal"
while :
do
    >&2 echo "stderr"
    >&2 echo "stderr"
    >&2 echo "stderr"
    sleep 1
done
exit $1
