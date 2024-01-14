---
title:    "PHP recipe: Generating random numbers"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why Generate Random Numbers?

When writing code, there are often times when we need to generate random numbers. Whether it's for testing, creating unique IDs, or any other number of reasons, having the ability to generate random numbers is a useful skill to have in your programming toolbox.

## How To Generate Random Numbers in PHP

Generating random numbers in PHP is a relatively simple task. We can use the built-in `rand()` function to generate a random number within a specified range. For example, if we wanted to generate a random number between 1 and 10, we could use the following code:

```PHP
$randomNumber = rand(1, 10);
echo $randomNumber; // Outputs a random number between 1 and 10
```

We can also use the `mt_rand()` function, which uses a slightly different algorithm and is considered to be a more secure way of generating random numbers. It also allows for a larger range of numbers to be generated.

```PHP
$randomNumber = mt_rand(100, 1000);
echo $randomNumber; // Outputs a random number between 100 and 1000
```

We can also use `random_int()` if we need to generate cryptographically secure random numbers. This function uses the system's random number generator and is considered to be the most secure option.

```PHP
$randomNumber = random_int(1000, 9999);
echo $randomNumber; // Outputs a cryptographically secure random number between 1000 and 9999
```

## Deep Dive into Generating Random Numbers

There are a few important things to keep in mind when generating random numbers in PHP. Firstly, it's important to remember that these functions are not truly random, but rather pseudo-random as they use algorithms to generate numbers. This means that the same seed value will always produce the same sequence of random numbers.

To avoid this, we can use `mt_rand()` with the `mt_srand()` function, which allows us to set a seed value for the random number generator. This will produce a different sequence of numbers each time the code is run.

```PHP
mt_srand(1234); // Sets the seed value to 1234
$randomNumber = mt_rand(1, 100);
echo $randomNumber; // Outputs a different random number each time the code is run
```

Another important factor to consider is the range of numbers that can be generated. PHP's `rand()` function has a maximum value of 2147483647, while `mt_rand()` and `random_int()` have a maximum value of 4294967295. If you need to generate larger numbers, you can use the `mt_getrandmax()` function to get the maximum value for the current random number generator.

See Also:

- [PHP Documentation on random number generation](https://www.php.net/manual/en/function.rand.php)
- [Tutorial on generating random numbers in PHP](https://www.tutorialspoint.com/php/php_random_number.htm)
- [Understanding random number generation in PHP](https://www.geeksforgeeks.org/understanding-random-number-generator-in-php/)