#!/bin/sh
curl -s https://sudoku.com/api/getLevel/expert | jq -r ".desc[0]"
