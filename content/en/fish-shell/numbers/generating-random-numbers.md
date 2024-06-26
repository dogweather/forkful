---
date: 2024-01-27 20:26:06.071881-07:00
description: "How to: Generating a random number in Fish can be straightforward, using\
  \ the combination of system utilities and shell capabilities. Below are some\u2026"
lastmod: '2024-03-13T22:45:00.468365-06:00'
model: gpt-4-0125-preview
summary: Generating a random number in Fish can be straightforward, using the combination
  of system utilities and shell capabilities.
title: Generating random numbers
weight: 12
---

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
