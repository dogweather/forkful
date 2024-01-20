---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenation in PHP is simply the process of joining multiple strings into one. As programmers, we do this to efficiently manage and manipulate text data within our applications.

## How To:

PHP uses the `.` operator for concatenation. Here's a simple example:

```PHP
$str1 = 'Hello';
$str2 = 'World';
$greeting = $str1 . ' ' . $str2;
echo $greeting;  // Outputs: Hello World
```

You can also use the `.=` operator to append data to an existing string:

```PHP
$message = 'Hello';
$message .= ' World';
echo $message;  // Outputs: Hello World
```

## Deep Dive

String concatenation has been a part of PHP since its inception, back when it was primarily a templating tool for HTML. The `.` operator – unusual but highly intuitive – was chosen to free up the `+` operator for mathematical operations exclusively.

It's worth noting PHP also offers additional techniques to embed variable values within a string:
* `sprintf()`: Allows formatting strings in a variety of ways.
* Interpolation: A double-quoted string lets you insert variable values directly.

```PHP
$name = 'World';
// Using sprintf
echo sprintf('Hello %s', $name);  // Outputs: Hello World
// Using interpolation
echo "Hello {$name}";  // Outputs: Hello World
```

PHP's string concatenation is memory-optimized; concatenating doesn't create new strings but builds on the previous one, reducing the need for memory reallocations.

## See Also

1. [PHP Manual: String Concatenation](https://www.php.net/manual/en/language.operators.string.php)
2. [PHP Manual: sprintf()](https://www.php.net/manual/en/function.sprintf.php)
3. [PHP Manual: String Interpolation](https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing)