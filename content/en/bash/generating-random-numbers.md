---
title:                "Bash recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why
Creating a random number generator may seem like a trivial task, but it has many uses in the world of programming. From simulating dice rolls in a game to generating test data for a database, having the ability to generate random numbers is a useful skill for any programmer.

## How To
To generate random numbers in Bash, we can use the `$RANDOM` variable, which generates a random number between 0 and 32767 each time it is called. Here's a simple example of how to use it to generate 5 random numbers:

```Bash
#!/bin/bash

for i in {1..5}
do
  echo $RANDOM
done
```

The output of this code would be 5 random numbers, similar to the following:

```
17754
28344
21377
7227
19492
```

We can also use the `shuf` command to generate a random permutation of a given list of numbers. This can be useful for things like creating unique IDs or shuffling a deck of cards. Here's an example of how to use `shuf` to generate a list of 10 unique numbers:

```Bash
#!/bin/bash

shuf -i 1-100 -n 10
```

The output of this code would be a list of 10 numbers between 1 and 100, in a random order. For example:

```
75
2
41
98
16
57
33
86
23
99
```

## Deep Dive
While the `$RANDOM` variable and `shuf` command are useful for simple random number generation, they are limited in their range and precision. For more complex and specific requirements, we can use the `awk` command to generate random numbers within a specified range, with a specific number of digits after the decimal point.

Here's an example of a `bash` function using `awk` to generate a random number between 10 and 100 with 2 decimal places:

```Bash
#!/bin/bash

function genRandom {
  awk 'BEGIN { srand(); print 10+rand()*90 }'
}

genRandom
```

The output of this code would be a random number like 59.27 or 30.89. By adjusting the `awk` command, we can generate random numbers with different ranges and precision.

## See Also
- [Bash documentation on `$RANDOM`](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)
- [Bash documentation on `shuf`](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)
- [Stack Overflow discussion on generating random numbers in Bash](https://stackoverflow.com/questions/25562494/generate-a-random-number-between-0-and-9-bash)