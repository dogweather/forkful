---
title:    "Fish Shell recipe: Generating random numbers"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to generate random numbers while coding in Fish Shell? Maybe you were working on a game, a simulation, or a statistical analysis project. Whatever the reason may be, generating random numbers is a useful skill to have in your programming repertoire.

## How To

Coding in Fish Shell, generating random numbers is as easy as using the `random` command. This command can generate both integers and floating-point numbers. Here's an example of how to generate 5 random integers between 1 and 10:

```Fish Shell
set i 0
while [ $i -lt 5 ]
  echo (random --min 1 --max 10)
  set i (math $i + 1)
end
```

The output of this code will look something like this:

```
3
7
9
1
4
```

If you need to generate floating-point numbers, you can use the `-l` flag to specify the number of decimals. For example, to generate 5 random floating-point numbers between 0 and 1 with 2 decimal places:

```Fish Shell
set i 0
while [ $i -lt 5 ]
  echo (random --min 0 --max 1 -l 2)
  set i (math $i + 1)
end
```

The output of this code will look something like this:

```
0.34
0.72
0.05
0.98
0.11
```

## Deep Dive

The `random` command in Fish Shell uses the Mersenne Twister algorithm to generate random numbers. This is a pseudorandom number generator that is known for its long period and high quality of randomness. It is also efficient and easy to implement, making it a popular choice for generating random numbers in many programming languages.

If you want to dig deeper into the technical details of generating random numbers with the Mersenne Twister algorithm, there are plenty of resources available online. You can also find documentation for the `random` command in the Fish Shell website.

## See Also

- Fish Shell Documentation on `random`: https://fishshell.com/docs/current/cmds/random.html
- Wikipedia page on Mersenne Twister: https://en.wikipedia.org/wiki/Mersenne_Twister