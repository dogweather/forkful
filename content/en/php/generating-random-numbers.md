---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Random numbers are numbers produced by a process whose outcome is unpredictable. Programmers generate such numbers for applications like simulations, cryptography, games, or for testing algorithms.

## How to:

To generate a random number in PHP, you'd use the `rand()` or `mt_rand()` functions. For instance:

```PHP
<?php
// Generate a random number between 1 and 10
$randomNumber = rand(1, 10);
echo $randomNumber;
?>
```

Looking a bit unpredictable, isn't it? Let's not stop there; let's use `mt_rand()`:

```PHP
<?php
// Generate a number between 20 and 50
$randomNumber = mt_rand(20, 50);
echo $randomNumber;
?>
```

`mt_rand()` works similar to `rand()`, but it's a bit faster and produces a better random value.

## Deep Dive

Initially in PHP, only `rand()` was available. But it was found that its algorithm has statistical issues and produces numbers that aren't evenly distributed. Enter Mersenne Twister's `mt_rand()`, a better pseudo-random number generator that brings better uniformity. 

For most ordinary purposes, like games or randomized content, `rand()` and `mt_rand()` will do. However, these are not cryptographically secure. For something truly random, such as generating cryptographic salts or keys, you'd use PHP’s `random_int()` or `random_bytes()` functions. 

```PHP
<?php
  // Generate a cryptographically secure random integer between 1 and 100
  $randomInt = random_int(1, 100); 
  echo $randomInt;
?>
```

This function gets us as close to true randomness as feasible.

## See Also

Want a deep dive into PHP's random number generation? Check these out:
- PHP Manual's [rand()](https://www.php.net/manual/en/function.rand.php)
- [mt_rand()](https://www.php.net/manual/en/function.mt-rand.php)
- [random_int()](https://www.php.net/manual/en/function.random-int.php)
- [random_bytes()](https://www.php.net/manual/en/function.random-bytes.php)