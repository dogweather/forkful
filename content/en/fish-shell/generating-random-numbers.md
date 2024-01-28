---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:00.562859-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in programming is about creating numbers that are unpredictable by human standards. Programmers use random numbers for tasks such as generating unique identifiers, selecting random elements from a list, or for simulations and gaming logic where unpredictability is essential.

## How to:

Fish Shell offers a straightforward way to generate random numbers using the `random` function. This function can generate random integers within a specified range.

```Fish Shell
# Generate a random number between 1 and 100
set random_number (random 1 100)
echo $random_number
```

You can also use it without specifying a range, in which case it defaults to generating a random number between 1 and 32767.

```Fish Shell
# Generate a random number with default range
set random_number_default (random)
echo $random_number_default
```

For scenarios requiring a random selection from an array, you can integrate `random` with other commands:

```Fish Shell
# Create an array
set animals (cat dog bird fish)

# Select a random index, arrays in Fish start at index 1
set random_index (random 1 (count $animals))

# Use the random index to select a random animal
echo $animals[$random_index]
```

## Deep Dive

The `random` function in Fish Shell is an internal built-in function, which means it's executed within the shell itself without calling external programs. This approach enhances the efficiency and speed of generating random numbers compared to shells that might rely on external utilities like `awk` or `shuf`.

Historically, generating random numbers in shell scripts often involved using external commands or delving into device files like `/dev/random` or `/dev/urandom`, which could introduce complexity or portability issues. Fish's inclusion of a native `random` function simplifies scripts and ensures consistency across different environments.

While Fish's `random` function is sufficient for most scripting needs, it's important to note that it may not be suitable for cryptographic purposes, where stronger, more unpredictable sources of randomness are required. In those cases, programmers might turn to external cryptographic tools or libraries designed specifically for security contexts.

In comparison to some other shells that lack built-in randomness functions, Fish's `random` offers a convenient and efficient way to incorporate randomness into scripts but always evaluate the suitability based on the task's specific requirements, especially where security is a concern.

## See also

### Official Fish Shell Documentation
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)

### Blog Posts and Tutorials
- **Stack Overflow Discussion**: [Generate a random number in Fish Shell](https://stackoverflow.com/questions/24815956/generate-a-random-number-in-fish-shell)
- **Command Line Tips**: [Using Math in Fish Shell](https://www.mankier.com/1/fish#Expressions-Using_Math_in_fish)
