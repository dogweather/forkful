---
title:    "PHP recipe: Generating random numbers"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why

Random numbers are a crucial aspect of programming, whether you are creating a game, performing simulations, or securing sensitive data. Being able to generate random numbers gives developers the ability to add an element of unpredictability to their code, making it more versatile and efficient. In this blog post, we will explore the different methods of generating random numbers in PHP.

## How To

Generating random numbers in PHP is a simple task. First, we will need to use the built-in random number generation function, `rand()`. This function generates a random integer between two given values.

```PHP
$random_number = rand(1, 10);
echo $random_number;
//Output: 7
```

We can also use `mt_rand()` function, which uses a better algorithm to generate random numbers compared to `rand()`.

```PHP
$random_number = mt_rand(50, 100);
echo $random_number;
//Output: 85
```

PHP also has a `random_int()` function, which generates random integers within a specified range, using a cryptographically secure algorithm. This is useful for generating random numbers for security purposes.

```PHP
$random_number = random_int(1000, 9999);
echo $random_number;
//Output: 3562
```

## Deep Dive

The `rand()` and `mt_rand()` functions use the libc random number generator, which may produce predictable results if not seeded. To avoid this, we can use the `srand()` function to seed the generator with a unique value.

```PHP
srand(time());
$random_number = rand();
echo $random_number;
```

It is worth noting that the `random_int()` function also takes care of seeding automatically, making it a more secure option for generating random numbers.

Another aspect to keep in mind is the range of values that can be generated. For `rand()` and `mt_rand()`, the minimum and maximum values should not exceed `PHP_INT_MAX` for optimal performance. For `random_int()`, the range can be up to `PHP_INT_MAX` on 64-bit systems, but it is limited to `PHP_INT_MAX - 1` on 32-bit systems.

## See Also

- [PHP: rand() function documentation](https://www.php.net/rand)
- [PHP: mt_rand() function documentation](https://www.php.net/manual/en/function.mt-rand.php)
- [PHP: random_int() function documentation](https://www.php.net/manual/en/function.random-int.php)