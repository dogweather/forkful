---
title:                "Fish Shell recipe: Generating random numbers"
programming_language: "Fish Shell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Generating random numbers is a common task in programming that is essential for various applications such as creating randomized tests, simulations, and games. By understanding how to generate random numbers using the Fish Shell, you can add an element of unpredictability to your code.

## How To

Coding a simple command to generate a random number in Fish Shell is straightforward. The syntax is as follows:

```Fish Shell
echo (math —random)
```

The output of this command will be a random decimal value between 0 and 1.

To generate a random integer within a specific range, we can add parameters to the `math-random` command. For example, if we want to generate a random number between 1 and 10, we can modify the command to this:

```Fish Shell
echo (math —random —min=1 —max=10)
```

The output from this command will be a random integer between 1 and 10.

We can also use the `seq` command to generate a list of random numbers. The syntax is as follows:

```Fish Shell
seq 10 | math —random
```

This command will generate 10 random decimal values and output them on separate lines.

## Deep Dive

Behind the scenes, the Fish Shell uses a pseudo-random number generator (PRNG) to generate random numbers. A PRNG is an algorithm that uses a seed value to generate a sequence of numbers that appear random. However, since the algorithm is deterministic, the same seed value will produce the same sequence of numbers. In Fish Shell, the seed value is based on the current time, meaning that every time you run the command, you will get a different sequence of numbers.

## See Also

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Generating Random Numbers in Other Programming Languages](https://www.geeksforgeeks.org/generating-random-numbers-in-java/)
- [Understanding Pseudo-Random Number Generators](https://www.khanacademy.org/computing/computer-science/cryptography/crypt/v/understanding-pseudo-random-number-generators)