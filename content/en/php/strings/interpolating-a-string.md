---
date: 2024-01-20 17:51:21.538405-07:00
description: 'How to: In PHP, you can interpolate strings using double quotes or heredoc
  syntax.'
lastmod: '2024-03-13T22:45:00.154024-06:00'
model: gpt-4-1106-preview
summary: In PHP, you can interpolate strings using double quotes or heredoc syntax.
title: Interpolating a string
weight: 8
---

## How to:
In PHP, you can interpolate strings using double quotes or heredoc syntax:

```php
$name = "World";
echo "Hello, $name!"; // Output: Hello, World!

// Using curly braces for more complex variables
$object = new stdClass();
$object->greeting = "Hello";
echo "{$object->greeting}, $name!"; // Output: Hello, World!

// Heredoc syntax for multi-line strings
$heredoc = <<<EOT
This is a string that contains $name within it.
You can write as much as you want here.
EOT;
echo $heredoc; // Output: This is a string that contains World within it.
```

Note: Single quotes won't interpolate:

```php
echo 'Hello, $name!'; // Output: Hello, $name!
```

## Deep Dive
Before PHP introduced interpolation, concatenation with the dot operator (.) was the way to go. For example:

```php
echo 'Hello, ' . $name . '!';
```

Interpolation streamlines this by parsing the variable directly within the string.

String interpolation has been around since PHP 4, but the use of complex expressions within curly braces became more flexible with PHP 7. With these improvements, PHP made it easier to embed any variable, including object properties and array elements, within a string.

Alternatives to interpolation exist, such as using `sprintf()` for formatted strings or `implode()` for arrays. These may sometimes offer more control over string formatting, especially for localization and complex structures.

Implementation-wise, PHP looks for variables inside strings when they are in double quotes or heredoc syntax and replaces them with the variable's value. The parser ignores the dollar sign ($) in single-quoted strings, treating it as a regular character.

## See Also
- [PHP: Strings](http://php.net/manual/en/language.types.string.php) - Official PHP documentation on strings.
- [PHP: Heredoc syntax](https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc) - PHP manual's detailed section on Heredoc.
- [PHP: String Operators](https://www.php.net/manual/en/language.operators.string.php) - More on string concatenation and the dot operator.
- [PHP: sprintf](https://www.php.net/manual/en/function.sprintf.php) - Documentation of the `sprintf()` function for string formatting.
