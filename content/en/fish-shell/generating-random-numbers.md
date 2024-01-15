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

## Why

Have you ever needed to generate random numbers for a project or program, but didn't want to hassle with complicated coding? Look no further! Using the Fish Shell, you can easily generate random numbers with just a few simple commands.

## How To

Coding examples and sample output are given below in code blocks.

```Fish Shell
#!/usr/bin/fish
# Generates 5 random numbers between 1 and 10
for i in (seq 1 5)
  echo (math random '(0+1)*10')
end
```
Output:
```
6.003245523
1.9087369164
3.4124398362
9.8653829416
5.3258921079
```

```Fish Shell
#!/usr/bin/fish
# Generates a random number between 0 and 100
echo (math random 0 100)
```
Output:
```
62.873593864
```

```Fish Shell
#!/usr/bin/fish
# Generates a random integer between 1 and 10
echo (math random '(0+1)*10' | string replace . '')
```
Output:
```
7
```

## Deep Dive

The `math random` command allows for the generation of random numbers in Fish Shell. The command takes in two optional arguments, a minimum and maximum value. If no arguments are given, `math random` will generate a random decimal between 0 and 1. By providing arguments, the command will generate a random number within the specified range. However, these numbers will still be in decimal form and may contain multiple digits after the decimal point. To generate whole numbers, the `string replace` command can be used to remove the decimal and any subsequent digits.

## See Also

- [Fish Shell Documentation](https://fishshell.com/docs/current/)
- [Random number generation in Bash](https://linuxize.com/post/bash-generate-random-number/)
- [Randomness in programming](https://en.wikipedia.org/wiki/Randomness_in_computing)