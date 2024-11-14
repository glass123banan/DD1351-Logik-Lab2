#!/bin/bash

# Script Name: run_verify.sh
# Description: Executes the SWI-Prolog verify command for a given test file.
# Usage: ./run_verify.sh <testname>
# Example: ./run_verify.sh valid01

# Check if exactly one argument is provided
if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <testname>"
  echo "Example: $0 valid01"
  exit 1
fi

# Assign the first argument to TESTNAME
TESTNAME="$1"

# Construct the file path
FILEPATH="tests/${TESTNAME}.txt"

# Check if the file exists
if [ ! -f "$FILEPATH" ]; then
  echo "Error: File '$FILEPATH' does not exist."
  exit 1
fi

# Execute the SWI-Prolog command
swipl -g "['beviskontroll'], verify('$FILEPATH')" -g halt

# Optional: Check the exit status of the Prolog command
if [ $? -eq 0 ]; then
  echo "Verification of '$FILEPATH' completed successfully."
else
  echo "Verification of '$FILEPATH' failed."
  exit 1
fi
