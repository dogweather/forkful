---
title:                "Handling errors"
date:                  2024-01-21T21:19:31.877074-07:00
model:                 gpt-4-1106-preview
simple_title:         "Handling errors"
programming_language: "PHP"
category:             "PHP"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/handling-errors.md"
---

{{< edit_this_page >}}

## What & Why?
Handling errors in PHP is about managing the unexpected—think typos, bad data, or network issues. Programmers do this to prevent crashes and give users helpful feedback.

## How to:

PHP offers a simple way to handle errors with `try`, `catch`, and `finally` blocks. Here's a quick look:

```php
<?php
function divide($dividend, $divisor) {
    if ($divisor == 0) {
        throw new Exception("Division by zero.");
    }
    return $dividend / $divisor;
}

try {
    echo divide(5, 0);
} catch (Exception $e) {
    echo "Caught exception: " . $e->getMessage();
} finally {
    echo "\nAlways executed.";
}
?>
```

Sample Output:
```
Caught exception: Division by zero.
Always executed.
```

## Deep Dive

Before PHP 7, error handling was a bit of a wild ride with `set_error_handler()` and the `@` operator to suppress errors. PHP 7 introduced throwable exceptions and errors, unifying the way we manage problems.

Alternatives to exceptions include returning special values like `false` or `null`, but this could get messy if you forget to check these return values.

An exciting part about error handling in PHP 7 and later is the introduction of typed exceptions, allowing for more specific catch blocks. A `try` block can have multiple `catch` blocks to handle different types of Exceptions separately.

Implementation detail: When an exception is thrown, PHP stops executing the current script at that point and jumps to the first matching `catch` block. If there is no matching catch, or if the exception is not caught, PHP throws a fatal error—so catch with care!

## See Also

- The PHP Manual on Exceptions: [php.net/manual/en/language.exceptions.php](https://www.php.net/manual/en/language.exceptions.php)
- PHP The Right Way on Error Reporting: [phptherightway.com/#error_reporting](https://phptherightway.com/#error_reporting)
- A deep dive into Exceptions in PHP: [web.dev/php-exceptions/](https://web.dev/php-exceptions/)