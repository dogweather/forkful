---
title:                "Generating random numbers"
aliases:
- /en/fish-shell/generating-random-numbers.md
date:                  2024-01-27T20:26:06.071881-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is a fundamental task in programming, used for everything from data sampling to game development. In Fish Shell, making use of system tools and built-in functions for this purpose allows programmers to incorporate randomness and variability into scripts and applications effectively.

## How to:

Generating a random number in Fish can be straightforward, using the combination of system utilities and shell capabilities. Below are some examples demonstrating how to generate random numbers within specified ranges.

**Generate a random number between 0 and 100:**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**Sample Output:**
```fish
42
```

**Generating a random number between any two numbers, say 50 and 150:**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**Sample Output:**
```fish
103
```

**Using random to shuffle a list:**

You might also want to randomly shuffle elements in a list. Here’s how you can do it:

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**Sample Output:**
```fish
C
A
E
D
B
```

Please note, the output will vary every time you run these commands due to the nature of randomness.

## Deep Dive

The Fish Shell `random` function provides an easy-to-use interface for generating pseudo-random numbers. Internally, it wraps around system-level random number generation utilities, offering a portable way to introduce randomness into your scripts. However, it's essential to remember that the randomness provided by `random` is sufficient for most scripting tasks but might not meet the cryptographic security requirements for applications needing a higher degree of unpredictability.

For high-stakes security contexts, consider using dedicated tools or programming libraries designed for cryptographic purposes, which provide stronger randomness guarantees. Nonetheless, for general scripting and applications where the highest security standards for randomness are not a requirement, Fish Shell's `random` function offers a convenient and effective solution.
