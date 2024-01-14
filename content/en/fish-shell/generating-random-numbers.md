---
title:                "Fish Shell recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

If you've ever needed to generate random numbers for a project or game, you may have realized that it can be a tedious and time-consuming task. That's where Fish Shell comes in. With its built-in random number generation function, you can easily generate random numbers without having to write your own code.

## How To

To generate a random number in Fish Shell, you can use the `random` command followed by the upper limit of the desired range. For example, if you want a random number between 1 and 10, you would use the following code:

```
Fish Shell
random 10
```

The output would be a random number between 1 and 10, such as 6 or 9. You can also use variables in the upper limit if you want a more dynamic range. For instance, if you want a random number between 1 and a user-defined maximum, you can use a variable like this:

```
Fish Shell
random $maximum
```

You can also specify a lower limit by using the `--lower-bound` flag. For example, if you want a random number between 50 and 100, you would use the following code:

```
Fish Shell
random --lower-bound 50 100
```

The output would be a random number between 50 and 100, such as 72 or 98.

## Deep Dive

The random number generation function in Fish Shell uses the Xorshift128+ algorithm, which is a fast and efficient method for generating random numbers. This algorithm generates high-quality random numbers with a long period, making it ideal for applications where a large number of random numbers are needed.

Furthermore, Fish Shell also has the ability to generate random strings using the `random string` command. This is particularly useful for generating unique IDs or passwords. You can specify the length of the string and even use special characters if needed.

## See Also

- [Fish Shell Documentation](https://fishshell.com/docs/current/)
- [Xorshift128+ Algorithm](https://en.wikipedia.org/wiki/Xorshift#xorshift*_128.2B)