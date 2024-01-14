---
title:    "Bash recipe: Generating random numbers"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Bash, short for "Bourne Again Shell", is a popular Unix shell used for automating tasks and interacting with the system. One interesting feature of Bash is its ability to generate random numbers, which can be useful for various purposes such as generating passwords, creating unique file names, or simulating randomized data.

## How To

To generate a random number in Bash, we can use the `RANDOM` variable. This variable can generate a random number between 0 and 32767 every time it is called. We can also use the `shuf` command, which generates a random permutation of its input. Here's an example of using both methods:

```Bash
echo $RANDOM #outputs a random number
shuf -i 1-10 -n 1 #outputs a random number between 1 and 10
```

The first line uses the `echo` command to print the value of the `RANDOM` variable. The second line uses the `shuf` command with the `-i` flag to specify a range of numbers (1-10) and the `-n` flag to specify the number of output numbers (1 in this case).

We can also use the `seq` command to generate a sequence of numbers and then use `shuf` to randomize them. This can be useful when we want to generate a specified number of unique random numbers. Here's an example of generating 10 unique random numbers between 1 and 100:

```Bash
shuf -i $(seq 1 100) -n 10
```

## Deep Dive

The `RANDOM` variable in Bash uses an algorithm to generate pseudo-random numbers. This means that the numbers are not truly random, but they appear random when called repeatedly. The algorithm used is called the linear congruential generator (LCG) and it produces a sequence of numbers based on a starting point, called the seed.

By default, the `RANDOM` variable in Bash uses the current time in seconds as the seed. This means that if you call the `RANDOM` variable multiple times in quick succession, you will get the same number. We can manually set the seed by using the `RANDOM` variable as the seed for `shuf`, like this:

```Bash
shuf -i 1-10 -n 1 --random-source=<(echo $RANDOM)
```

This will use the value of the `RANDOM` variable to generate a unique random number every time.

## See Also

- More information on using the `RANDOM` variable: https://www.gnu.org/software/bash/manual/html_node/Shell-Variables.html#index-RANDOM
- `shuf` command reference: https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html
- `seq` command reference: https://www.gnu.org/software/coreutils/manual/html_node/seq-invocation.html