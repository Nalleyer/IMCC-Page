#!/bin/bash

echo "BUILDING..."
npm run build
echo "COPYING..."
cp ./index.html ../imcc-back/static/
cp -r ./dist ../imcc-back/static/
echo "DOWN"
