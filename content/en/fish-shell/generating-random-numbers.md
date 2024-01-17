---
title:                "Generating random numbers"
html_title:           "Fish Shell recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is a key function in programming that allows developers to create unpredictable and unbiased values. This is important for tasks such as generating unique IDs, creating randomized simulations, and creating game elements such as dice rolls. By incorporating random number generation into their code, programmers can add an element of chance and surprise to their applications.

## How to:

Fish Shell has a simple built-in command for generating random numbers, `random`. It takes two optional parameters: a minimum value and a maximum value. If both parameters are omitted, the default values are 0 and 32767.

```
# Generate a random number between 1 and 10
Fish Shell> random 1 10
5
# Generate a random number between 0 and 100
Fish Shell> random
42
```

## Deep Dive:

Random number generation has been a topic of study in mathematics for centuries, and it's an essential component of various computer algorithms. There are alternative methods for generating random numbers, such as using pseudo-random number generators or utilizing atmospheric noise. However, the `random` command in Fish Shell uses a linear congruential generator (LCG) algorithm, which is a simple and efficient method for generating random numbers.

Fish Shell also offers the `uuidgen` command for creating universally unique identifiers (UUIDs), which are essentially random numbers with a specific structure. These are commonly used for creating unique IDs for database entries or for tracking individual devices.

## See Also:

For more information on generating random numbers in Fish Shell, check out the official documentation:
https://fishshell.com/docs/current/cmds/random.html

To learn more about the LCG algorithm used in the `random` command, here's a detailed explanation:
https://en.wikipedia.org/wiki/Linear_congruential_generator

For a deeper dive into the importance and applications of random numbers in computer science, this article provides a good overview:
https://www.investopedia.com/terms/r/random-number-generator.asp