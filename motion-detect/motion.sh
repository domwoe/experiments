#!/bin/bash
echo "Directory scanned for motion detection: $1"
find $1 -type f -regex "[^_]+\.avi$" -print -exec python motion.py {} \;
