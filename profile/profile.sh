#!/bin/bash
# Usage: ./run_experiments.sh <number_of_tests>
# Example: ./run_experiments.sh 10

set -euo pipefail

# Check if number of tests is provided
if (( $# < 1 )); then
    echo "Usage: $0 <number_of_tests>"
    exit 1
fi

FAST=1
if (( $# >= 2 )) && [[ "$2" == "-s" ]]; then
    FAST=0
fi

N="$1"

# List of program names to test.
# Update these names with the actual executable names.
# programs=("cpp-fen.exe" "hs-standalone.exe" "hs-library-v1.exe" "hs-library-v2.exe" "hs-library.exe" )
programs=("cpp-fen.exe" "hs-standalone.exe" "hs-library.exe" "hs-library-v3.exe")


make "${programs[@]}"

# Print table header.
printf "|%-10s|" "Testcase"
for prog in "${programs[@]}"; do
    printf "%-20s|" "$prog"
done
printf "\n"

# Loop over each test case.
for ((i = 1; i <= N; i++)); do
    testcase_file="testcase_${i}.txt"
    
    # Generate test case and save to file.
    (( $FAST )) || pypy3 generator.py > "$testcase_file"
    
    # Print test case number.
    printf "|%-10s|" "$i"
    
    # For each program, run the test case and measure runtime.
    for prog in "${programs[@]}"; do
        # Get start time (seconds.nanoseconds).
        start=$(date +%s.%N)
        
        # Run the program with the test case as input.
        # Output is redirected to /dev/null.
        taskset -c 7 ./"$prog" < "$testcase_file" > /tmp/output
        
        # Get end time.
        end=$(date +%s.%N)
        
        # Compute the elapsed time.
        runtime=$(echo "$end - $start" | bc)
        
        # Print runtime formatted into a table cell.
        printf "%-20.3f|" "$runtime"
    done
    # End the current row.
    printf "\n"
done
