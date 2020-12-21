#!/bin/bash

pushd test > /dev/null || exit
curl -X POST 127.0.0.1:3000/messages -d @data.json
popd > /dev/null || exit
printf "\n"
