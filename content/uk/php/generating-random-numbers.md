---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:49:40.472328-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Навіщо?)
Random numbers in PHP are like spices in borscht—essential. They're used in games, simulations, and security. It's about unpredictability and chance.

## How to: (Як робити:)
```PHP
<?php
// Basic random number
echo rand(1, 100);  // Outputs a number between 1 and 100

// Using mt_rand() for a faster, better random number
echo mt_rand(1, 100);  // Similarly, outputs a number between 1 and 100

// Cryptographically secure random number in PHP 7 and above
echo random_int(1, 100);  // More secure random number generation
?>
```
Sample output:
```
56
78
29
```

## Deep Dive (Занурення у Глибину):
Did you know `rand()` is old news? It's been around since PHP 4. Enter `mt_rand()`, which is faster and has a better randomization algo. For security that doesn't mess around, `random_int()` is your guy, using a cryptographically secure pseudo-random number generator (CSPRNG).

Alternatives? Well, if you're into cryptography, PHP's `openssl_random_pseudo_bytes()` is another option, but it's binary data, so you better know your way around bits and bytes.

Implementation-wise, random functions tap into your system's random number generator. But, consistency isn't exactly global; different systems, different quality of randomness.

## See Also (Дивіться також):
- PHP Manual on `rand()`: [php.net/manual/en/function.rand.php](https://www.php.net/manual/en/function.rand.php)
- PHP Manual on `mt_rand()`: [php.net/manual/en/function.mt-rand.php](https://www.php.net/manual/en/function.mt-rand.php)
- PHP Manual on `random_int()`: [php.net/manual/en/function.random-int.php](https://www.php.net/manual/en/function.random-int.php)
- Article on randomness in cryptography: [Wikipedia - Cryptographic Nonce](https://en.wikipedia.org/wiki/Cryptographic_nonce)