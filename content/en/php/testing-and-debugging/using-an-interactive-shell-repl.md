---
date: 2024-01-25 03:39:48.723364-07:00
description: 'How to: Launch the PHP REPL by running `php -a` in your terminal. Here''s
  a taste of how it works.'
lastmod: '2024-03-13T22:45:00.168306-06:00'
model: gpt-4-1106-preview
summary: Launch the PHP REPL by running `php -a` in your terminal.
title: Using an interactive shell (REPL)
weight: 34
---

## How to:
Launch the PHP REPL by running `php -a` in your terminal. Here's a taste of how it works:

```php
php > echo "Hello, World!";
Hello, World!
php > $arr = [1, 2, 3];
php > print_r($arr);
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

You can define functions, too:

```php
php > function sum($a, $b) { return $a + $b; }
php > echo sum(5, 10);
15
```

## Deep Dive
REPLs have been around in some form since the early days of LISP in the 1960s. PHP's interactive shell is less advanced compared to those of languages like Python or JavaScript. It doesn't persist state between sessions and lacks features like auto-completion. For a more feature-rich PHP REPL, consider alternatives like `psysh` or `boris`. These third-party shells offer better introspection tools, tab-completion, and even a debugger.

Under the hood, PHP's REPL works by compiling and executing each line of code as it's entered. The limitations of this approach become clear with things like redeclaring classes, which isn't possible in the same session. It's great for simple tests but can get cumbersome for complex tasks.

## See Also
- [PHP Manual - Interactive shell](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH: A runtime developer console, interactive debugger and REPL for PHP](https://psysh.org/)
- [Boris: A tiny REPL for PHP](https://github.com/borisrepl/boris)
