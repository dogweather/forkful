---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:48:57.765230-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Done right, random numbers add unpredictability to apps, like for games or security. Programmers need them for simulations, testing, or when outcomes mustn't be biased.

## How to (Як це зробити):
### Simple Random Number:
Generate a random number between 1 and 10 in Fish.
```Fish Shell
set random_number (random 1 10)
echo $random_number
```
#### Sample output:
```
7
```

### Random Number within a Range:
Say you want a number between 5 and 15.
```Fish Shell
set range_min 5
set range_max 15
set random_number (random $range_min $range_max)
echo $random_number
```
#### Sample output:
```
11
```

### Generate a Random String:
Use `random` with `tr` to get a random string. This example gives you 8 characters long string.
```Fish Shell
set random_string (random 0x100000000 | tr -dc A-Za-z0-9 | head -c8)
echo $random_string
```
#### Sample output:
```
4b2N7a9Q
```

## Deep Dive (Поглиблений Розвід):
Once upon a time, randomness in computers was a tough nut. Early methods weren't so random. Now we've got algorithms that simulate randomness quite convincingly, though they're technically pseudorandom.

Fish uses `/dev/urandom` for seeding its `random` function – it’s good enough for non-cryptographic uses. If security is key, consider different tools, like OpenSSL or `/dev/random` though the latter might block if it runs dry of 'entropy'.

Alternatives to Fish `random` include other shells' built-in functions, or dedicated programs like `shuf`, `awk`, and `perl`.

Regarding implementation, Fish's `random` is a front end to standard C library functions. It handles seeding and value generation so you don't have to think about it.

## See Also (Дивіться також):
- Fish Shell documentation: [Fish: Commands](https://fishshell.com/docs/current/commands.html#random)
- Randomness and computers: [Wikipedia: Random number generation](https://en.wikipedia.org/wiki/Random_number_generation)
- More advanced random tasks in Unix-like systems: [Advanced Bash-Scripting Guide: Random Variables](https://tldp.org/LDP/abs/html/randomvar.html)