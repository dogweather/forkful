---
date: 2024-01-20 17:56:36.994318-07:00
description: 'How to: PHP uses a global array `$argv` to store command line arguments,
  with `$argv[0]` being the script name. Here''s how you use it.'
lastmod: '2024-03-13T22:45:00.181120-06:00'
model: gpt-4-1106-preview
summary: PHP uses a global array `$argv` to store command line arguments, with `$argv[0]`
  being the script name.
title: Reading command line arguments
weight: 23
---

## How to:
PHP uses a global array `$argv` to store command line arguments, with `$argv[0]` being the script name. Here's how you use it:

```php
<?php
// check if any arguments are passed
if ($argc > 1) {
    echo "Hello, " . $argv[1] . "!\n";
} else {
    echo "Hello, whoever you are!\n";
}
?>
```

If you call this script `sayhello.php` and run `php sayhello.php World`, the output will be:

```
Hello, World!
```

No arguments? You'll get:

```
Hello, whoever you are!
```

## Deep Dive
Historically, command line scripts have been the bedrock of system automation, long before GUIs took over. PHP, although widely used for web development, also provides robust CLI support. 

Two main ways to read arguments in PHP are `$argv` and the `getopt()` function. The former is a simple array while `getopt()` provides more complex functionality, like parsing options (with or without values).

As for implementation, `$argv` and `$argc` (the argument count) are automatically available in CLI mode — no need for extra setup. They’re not present when running PHP web scripts because that's not their arena.

But remember, if you register `argv` and `argc` as global variables through `php.ini` or server configuration, they can also be accessed in web scripts. Though, that's rarely needed and could be a security risk.

## See Also
For more complex command line parsing:
- [PHP.net getopt](https://www.php.net/manual/en/function.getopt.php)

To dive into PHP's CLI server:
- [PHP.net Command line usage](https://www.php.net/manual/en/features.commandline.php)

Engage with the PHP community:
- [PHP CLI discussions on Stack Overflow](https://stackoverflow.com/questions/tagged/php+cli)
