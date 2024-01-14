---
title:                "PHP recipe: Writing to standard error"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered an error while working on a PHP project and had no idea what went wrong? Don't worry, we've all been there. But have you considered using standard error in your code to help you catch and debug errors more efficiently? Writing to standard error is a technique used by many developers to display error messages, making it easier to identify and fix bugs in their code. In this blog post, we'll discuss why writing to standard error is beneficial and how you can implement it in your PHP projects.

## How To

Writing to standard error in PHP is quite simple. All you need to do is use the `fwrite()` function and specify `STDERR` as the first argument. Let's take a look at an example:

```PHP
<?php
$file = "sample.txt";

// open file for writing
$handle = fopen($file, "w");

// if opening file failed, write error message to standard error
if (!$handle) {
    fwrite(STDERR, "Error opening file for writing");
} else {
    // write content to file
    fwrite($handle, "Hello world!");
    fclose($handle);
}
```

In the example above, we use the `STDERR` constant to specify that the error message should be written to standard error instead of standard output. This helps to differentiate between regular output and error messages, making it easier to locate the issue.

Now, let's see what the output looks like when we run this code:

```
$ php sample.php
Error opening file for writing
```

As you can see, the error message is displayed on the screen, making it clear that there was an issue with opening the file for writing. This saves us time and effort in trying to figure out the problem.

## Deep Dive

Now, let's delve a bit deeper into writing to standard error. One of the main benefits of using standard error is that it allows you to log errors, making it easier to troubleshoot and fix issues in your code. In addition, using standard error also helps to ensure that error messages are displayed to the user, even if they have disabled regular output.

PHP also provides the `error_log()` function, which can be used to write error messages to a specified log file. This is especially useful for larger projects where you may need to track and analyze error messages. You can also customize the format of the log file to include additional information such as timestamp and the source of the error.

Another advantage of using standard error is that it allows you to handle different types of errors separately. For example, you can use `STDERR` for fatal errors that cause the script to terminate, while using the `error_log()` function for non-fatal errors that can be handled without stopping the script.

## See Also

- [PHP Error Handling](https://www.php.net/manual/en/book.errorfunc.php)
- [PHP fwrite() function](https://www.php.net/manual/en/function.fwrite.php)
- [PHP error_log() function](https://www.php.net/manual/en/function.error-log.php)

In conclusion, writing to standard error in PHP is a useful technique that can help you to efficiently debug and troubleshoot errors in your code. It's simple to implement and can save you time and effort in fixing bugs. So next time you encounter an error, consider using standard error to make your debugging process a little smoother.