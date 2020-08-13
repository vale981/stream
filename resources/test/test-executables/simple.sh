#!/bin/sh
echo "normal"
>&2 echo "stderr"
>&2 echo "stderr"
>&2 echo "stderr"

exit $1
