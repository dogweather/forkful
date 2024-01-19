---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string means transforming the string into a format that a computer can comprehend as a date. We need this because computers, unlike humans, don't understand the variety of ways dates can be written.

## How to:

PHP has a built-in function called `strtotime()` for this task. Stick a string into it, and out comes a time-stamp. It's quite magical. Here's how we do it:

```PHP
$date = "15th January 2023";
$timestamp = strtotime($date);
echo date('d-m-Y', $timestamp);
```

This script will output: `15-01-2023`.

Or let's take a date string with a different format:

```PHP
$date = "2023/01/15";
$timestamp = strtotime($date);
echo date('d-m-Y', $timestamp);
```

And here you'll get the exact same output, `15-01-2023`, despite the different input format.

## Deep Dive

`strtotime()` actually has a heap of history. It was born in PHP 4 as 'Magpie' for its ability to pick out dates regardless of format. Over time, it's evolved and matured, just like a fine wine.

Alternatives, you ask? Well, PHP's `DateTime` class is another way to parse dates. It's object-oriented, offering more flexibility if you're doing more complex operations with dates.

Implementation-wise, `strtotime()` internally uses the `Parsedate` library, which is why it handles such a wide array of formats.

## See Also

Boost your PHP knowledge with these resources:

1. PHP's official Date/Time documentation: https://www.php.net/manual/en/book.datetime.php
2. Excellent strtotime() reference guide: https://www.php.net/manual/en/function.strtotime.php
3. Learn more about the `DateTime` class: https://www.php.net/manual/en/class.datetime.php

Remember, practice makes perfect. Happy coding!