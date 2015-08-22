#!/bin/bash
#This script searches VG.no for the word specified as an argument
wget -q http://www.vg.no
sed -n 's/.*href="http:\/\/\([^"]*\).*/\1/p' index.html | grep -i "$1" > tmp.html
url=$(head -n 1 tmp.html)
if [ -z "$url" ]; then
    echo "No match found on VG.no :("
else
echo "Opening up $url in firefox"
/usr/bin/firefox -q -new-window $url
fi
rm tmp.html
rm index.html
