---
date: 2024-01-25 02:59:58.939306-07:00
description: "Organizing code into functions is about breaking down your code into\
  \ reusable blocks with defined purposes. We do it to keep things tidy, prevent\u2026"
lastmod: '2024-03-13T22:45:00.171969-06:00'
model: gpt-4-1106-preview
summary: Organizing code into functions is about breaking down your code into reusable
  blocks with defined purposes.
title: Organizing code into functions
weight: 18
---

## What & Why?
Organizing code into functions is about breaking down your code into reusable blocks with defined purposes. We do it to keep things tidy, prevent redundancy, and make debugging a breeze.

## How to:
Imagine we've got repetitive code for greeting users. Instead, we'll wrap it in a function like `greet_user`:

```php
function greet_user($name) {
    return "Hello, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

Output:
```
Hello, Alice!
Hello, Bob!
```

Now, you've got a handy tool you can use anytime without rewriting the same lines of code every time you want to say hello.

## Deep Dive
Functions have been in programming since the early days of FORTRAN in the '50s. They're a cornerstone of structured programming and are all about modularity and isolation. Alternatives? Well, you can go object-oriented and talk classes and methods, which are functions with a fancy suit on. As for PHP, implementation details include specifying default values for parameters, type hinting for inputs, and being able to return multiple values by using an array or, from PHP 7.1 onwards, a list.

Here's a modern twist with type declaration and default values:

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4 brought arrow functions too, helping to write concise one-liner functions, commonly used in array operations:

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

Output:
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## See Also
- [PHP Manual on Functions](https://www.php.net/manual/en/functions.user-defined.php)
- [PHP: The Right Way - Functions](https://phptherightway.com/#functions)
- [Learn about PHP 7.4 Arrow Functions](https://stitcher.io/blog/short-closures-in-php)
