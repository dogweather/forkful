---
title:                "PHP recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to generate random numbers in your PHP programming? Maybe you're creating a game or a random generator for a website. Whatever the reason may be, generating random numbers in PHP can be a useful skill in your coding arsenal.

## How To

Generating random numbers in PHP is fairly simple. There is a built-in function called `rand()` that allows you to generate random numbers within a specified range. Let's take a look at an example:

```PHP
//Generate a random number between 1 and 10
$rand_num = rand(1, 10);

echo "The random number is: " . $rand_num;
```

The output of this code would be a random number between 1 and 10, like 7 or 3. You can also specify a single parameter to generate a random number between 0 and that number, like `rand(20)` would generate a random number between 0 and 20.

You can even use `rand()` to simulate a coin toss by generating a random number between 0 and 1, with 0 representing heads and 1 representing tails. Here's an example:

```PHP
//Simulate a coin toss
$coin = rand(0, 1);

if ($coin == 0) {
  echo "Heads";
} else {
  echo "Tails";
}
```

The output of this code would be either "Heads" or "Tails", depending on the value generated for `$coin`.

## Deep Dive

The `rand()` function uses a pseudo-random algorithm to generate the random numbers. This means that the numbers are not truly random, but they appear to be random. This is because computers are deterministic machines and cannot generate true randomness.

If you need a more secure way to generate random numbers, PHP also has a `random_int()` function which uses a cryptographically secure algorithm to generate random numbers. This is useful for things like generating password salts or creating secure tokens.

## See Also

To learn more about generating random numbers in PHP, check out these resources:

- [PHP Manual: rand()](https://www.php.net/manual/en/function.rand.php)
- [PHP Manual: random_int()](https://www.php.net/manual/en/function.random-int.php)
- [W3Schools: PHP rand() Function](https://www.w3schools.com/php/func_math_rand.asp)
- [SitePoint: How to Generate Random Numbers in PHP](https://www.sitepoint.com/how-to-generate-random-numbers-in-php/)

Now go forth and add some randomness to your PHP projects!