---
title:                "Generating random numbers"
date:                  2024-01-20T17:49:45.377639-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Random numbers in PHP? Yep, they make your programs unpredictable. Handy for games, simulating scenarios, or security (like salts in encryption).

## How to:

### Basic Random Number

Get a simple random integer between 0 and 100.
```php
echo rand(0, 100);
```
Sample output: `42`

### Better Randomness

Need more security? Use `random_int()` instead. It's cryptographically secure.
```php
echo random_int(0, 100);
```
Sample output: `57`

### Random Bytes

Generating random bytes, often for cryptographic use:
```php
echo bin2hex(random_bytes(5));
```
Sample output: `3f7a2b1d3c`

### Seeding the Generator

As of PHP 7.1, `rand()` and `mt_rand()` no longer require manual seeding. It's now automatic and more secure!

## Deep Dive

Back in the day, random numbers in PHP were less random than you'd want (talking pre-PHP 7 here). `rand()` was okay for simple tasks but not for anything needing real unpredictability. That's where `mt_rand()`, based on Mersenne Twister algorithm, came in - better, but still not good for crypto-stuff.

Enter PHP 7, and things got serious. The random functions got an overhaul. `random_int()` and `random_bytes()` were introduced, wielding cryptographically secure randomness care of your operating system's random number generator (CSPRNG). Now that's proper randomness.

Why not always use `random_int()`? Performance. It's slower than `rand()` due to the added security. So, pick what fits your needs.

Lastly, on seeding: That's like starting a random number pattern. Old PHP made you do it yourself, leading to predictable patterns if done wrong. Now PHP handles it invisibly and way better. 

## See Also

- PHP Manual on Random Integers: [PHP: random_int - Manual](https://www.php.net/manual/en/function.random-int.php)
- PHP Manual on Random Bytes: [PHP: random_bytes - Manual](https://www.php.net/manual/en/function.random-bytes.php)