# Script Name: run_verify.sh
# Description: Executes the SWI-Prolog verify command for a given test file.
# Usage: ./run_verify.sh <testname>
# Example: ./run_verify.sh valid01

# Check if exactly one argument is provided
if [ "$#" -ne 1 ]; then
  echo "Usage: . run.sh <testname>"
  echo "Example: . run.sh valid01"
else

    # Assign the first argument to TESTNAME
    TESTNAME="$1"

    # Construct the file path
    FILEPATH="tests/${TESTNAME}.txt"

    # Check if the file exists
    if [ ! -f "$FILEPATH" ]; then
    echo "Error: File '$FILEPATH' does not exist."
    fi

    # Execute the SWI-Prolog command
    swipl -g "['beviskontroll.pl'], verify('$FILEPATH')" -g halt

    # Optional: Check the exit status of the Prolog command
    if [ $? -eq 0 ]; then
    echo "Verification of '$FILEPATH' completed successfully."
    else
    echo "Verification of '$FILEPATH' failed."
    fi
fi
