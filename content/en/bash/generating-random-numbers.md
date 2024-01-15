---
title:                "Generating random numbers"
html_title:           "Bash recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why
Are you looking to add a bit of unpredictability to your Bash scripts? Or maybe you just want to generate some randomized data for testing purposes? Whatever your reason may be, learning how to generate random numbers in Bash can be a useful skill to have.

## How To
```Bash
# Generate a random integer between 0 and 10
echo $(( RANDOM % 11 ))

# Generate a random number between 0 and 1 with 2 decimal places
echo $(printf "%.2f" $RANDOM/32767)

# Generate a random string of 10 characters
cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 10 | head -n 1
```
Sample output:
```
7
0.81
uSfXbYqL1I
```

## Deep Dive
Bash offers several different methods for generating random numbers. The first example above uses the built-in `$RANDOM` variable which returns a random number between 0 and 32767. By using the modulus operator, we can limit the range of the random number to our desired range.

The second example demonstrates using the `printf` function to format a random number into two decimal places. The `$RANDOM` variable generates a random integer, so we divide it by 32767 (which is the maximum value) and then use `printf` to specify the number of decimal places.

Lastly, the third example uses the `cat` and `tr` commands to extract random characters from the `/dev/urandom` file. Then, the `fold` command limits the character count to 10 and `head` ensures we only get one output.

See Also
- [Bash Variables Reference](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)
- [Bash Arithmetic Expansion](https://www.gnu.org/software/bash/manual/html_node/Arithmetic-Expansion.html)
- [Bash Text Manipulation Commands](https://www.gnu.org/software/bash/manual/html_node/Text-Manipulation.html)