---
title:                "Generating random numbers"
html_title:           "PHP recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is the process of creating a value that is not predictable and occurs by chance. Programmers use this technique to simulate real-world scenarios like rolling dice, shuffling cards, or generating passwords. Random numbers are also essential in data encryption and statistical analysis.

## How to:

To generate random numbers in PHP, we can use the built-in function `rand()`. This function takes two parameters, the `min` and `max` values, and returns a random number between them.

```PHP
// Generate a random number between 1 and 10
$randomNumber = rand(1, 10);
```

We can also use the `mt_rand()` function, which uses a better random number generator algorithm and is faster than `rand()`. It takes the same parameters as `rand()`.

```PHP
// Generate a random number between 100 and 1000
$randomNumber = mt_rand(100, 1000);
```

To get a random number with decimal places, we can use the `rand()` or `mt_rand()` function, and divide the result by a specific value.

```PHP
// Generate a random number between 0.0 and 1.0
$randomNumber = rand() / getrandmax();

// Generate a random number between 0.0 and 5.0
$randomNumber = mt_rand() / mt_getrandmax() * 5;
```

We can also generate random numbers from an array using the `array_rand()` function.

```PHP
$numbers = [1, 2, 3, 4, 5];
// Get a random number from the array
$randomNumber = $numbers[array_rand($numbers)];

// Get a random key from the array
$randomKey = array_rand($numbers);
```

## Deep Dive:

Random number generators (RNG) have been around since the 1940s, with the invention of the electronic computer. The randomness of numbers has improved over the years with more sophisticated algorithms, such as the Mersenne Twister used in `mt_rand()`. 

An alternative to built-in PHP functions is using external sources for generating random numbers, such as APIs or libraries like Random.org and OpenSSL.

The randomness of numbers in RNG is not truly random since it is still based on algorithms and seed values. These seed values are used to initialize the generator and can affect the sequence of numbers generated. In PHP, we can set the seed value using `srand()` function to get the same sequence of random numbers every time.

## See Also:

- [PHP Documentation on rand()](https://www.php.net/manual/en/function.rand.php)
- [PHP Documentation on mt_rand()](https://www.php.net/manual/en/function.mt-rand.php)
- [Random.org API](https://www.random.org/clients/http/)
- [OpenSSL Library - Random Number Generation](https://www.php.net/manual/en/openssl.random-pseudo-bytes.php)