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

## What & Why?

Generating random numbers is the process of producing a sequence of numbers that have no discernable pattern. This can be a useful tool for programmers in a variety of applications, such as cryptography, statistical simulations, and game development.

## How to:

```Bash
#To generate a random integer between 1 and 10:
echo $((1 + RANDOM % 10))

#To generate a random decimal between 0 and 1:
echo $(awk -v n=2 -v r="$RANDOM" 'BEGIN{srand(r);printf "%.2f\n",rand()/n}')

#To generate a secure random password with 12 characters:
< /dev/urandom tr -dc A-Za-z0-9 | head -c12
```

Sample output:
- Random integer: 5
- Random decimal: 0.31
- Random password: Af8sRb32Gh9E

## Deep Dive:

When computers were first developed, there was no way for them to generate truly random numbers. Instead, algorithms were used to produce "pseudo-random" numbers that appeared random, but were actually determined by a mathematical formula. However, with the advancement of technology, computers can now generate truly random numbers by using sources of entropy such as keystrokes and system noise.

There are also other programming languages, such as Python and Java, that have built-in functions for generating random numbers. These functions typically use more advanced and secure algorithms than the one used in Bash.

The implementation of generating random numbers in Bash is done through the use of built-in variables, such as $RANDOM, which produces a random integer between 0 and 32767. This value can then be manipulated to fit the desired range or format.

## See Also:

- [Bash manual](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameters.html#Shell-Parameters)
- [Random Numbers in Python](https://docs.python.org/3/library/random.html)
- [Random Numbers in Java](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)