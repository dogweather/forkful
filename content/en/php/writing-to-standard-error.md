---
title:                "PHP recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Standard error is an essential part of programming in PHP. It allows developers to capture error messages and debug their code, making it easier to identify and fix any issues that may arise. In this blog post, we will explore how to write to standard error in PHP and why it is crucial for every developer to know.

## How To

Writing to standard error in PHP is a simple, yet powerful tool that can greatly improve your debugging process. In PHP, the standard error stream is represented by the constant `STDERR`. To write to this stream, we can use the `fwrite()` function, passing in the `STDERR` constant as the first argument. Let's take a look at an example:

```PHP
<?php
$number = 0;
if ($number == 0) {
    // write error message to standard error stream
    fwrite(STDERR, "Cannot divide by zero!");
}
```

In the above code, we are checking if the variable `$number` is equal to 0. If it is, we use the `fwrite()` function to write an error message to the standard error stream. This message will be displayed in the console or written to a log file, depending on how you have configured your server.

You can also use the `error_log()` function to write to standard error. This function is useful for logging errors in production environments. Here's an example:

```PHP
<?php
$number = 0;
if ($number == 0) {
    // write error message to standard error stream
    error_log("Division by zero!");
}
```

The `error_log()` function will write the error message to the configured error log file for your PHP installation.

## Deep Dive

Now that we have seen how to write to standard error in PHP, let's dive deeper into why it is important. When writing a large codebase, it is inevitable that errors will occur at some point. These errors can be difficult to identify and fix without proper debugging techniques.

By writing to standard error, we can capture these errors and view them in the console or log files, making it easier to locate the source of the problem. This saves time and effort during the development process and ensures that our code is free of errors before deploying it to production.

In addition to logging errors, writing to standard error can also be useful for displaying important information during execution. This can be helpful for tracking the flow of your code and understanding any unexpected behavior.

## See Also

- [PHP Error Handling](https://www.php.net/manual/en/book.errorfunc.php)
- [Debugging in PHP](https://www.php.net/manual/en/book.debugger.php)
- [Error Logging in PHP](https://www.php.net/manual/en/function.error-log.php)