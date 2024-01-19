---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in programming involves creating a sequence that can't be predicted logically. We use random number generation for tasks like creating unique identifiers, implementing game logic, or introducing randomness in machine learning data sets.

## How to

Creating a random number in Bash is a piece of cake! Bash provides a built-in `$RANDOM` variable. Check out the example below:

```Bash
echo $RANDOM
```

Running the above command will yield a random number between 0 and 32767. For bigger numbers or to specify a range, you can do simple math with `$RANDOM`. For a random number between 1 and 100, you can do:

```Bash
echo $(($RANDOM % 100 + 1))
```

`$RANDOM % 100` results in a number between 0 and 99, adding 1 shifts the range to be 1 and 100.

## Deep Dive

Historically `$RANDOM` is a pseudo-random number generator introduced by Bash, which comes with a limitation. The generated numbers are not cryptographically strong due to predictability. 

In those cases where strength matters, you could use `/dev/random` or `/dev/urandom`. Here's an example to get a random number using `/dev/urandom`:

```Bash
od -An -N2 -i /dev/urandom
```

Lastly, since `bash 4.2-alpha`, the `$RANDOM` variable is no longer based on the current time, making it more unpredictable.

## See Also

1. Bash Guide for Beginners: https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_09_02.html
2. Advanced Bash-scripting Guide: https://tldp.org/LDP/abs/html/randomvar.html
3. Generate Random Numbers in Linux: https://www.binarytides.com/linux-command-random-number/