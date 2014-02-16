#!/bin/bash
find . -type f -regex "[^_]+\.avi$" -print -exec python motion.py {} \;
