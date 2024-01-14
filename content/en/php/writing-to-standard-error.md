---
title:    "PHP recipe: Writing to standard error"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why

Writing to standard error is an essential part of PHP programming because it allows for the proper handling of errors and debugging of code. By writing error messages to standard error, developers can easily identify and fix issues in their code, making for more efficient and reliable programs.

## How To

Writing to standard error in PHP is a simple task that can be accomplished using the `fwrite()` function. This function takes two parameters: the file pointer to standard error (`STDERR`) and the error message that needs to be written. Let's take a look at an example:

```PHP
<?php
//open standard error file pointer
$stderr = fopen('php://stderr', 'w');

//error message to be written
$error = "An error has occurred: Invalid input\n";

//write error message to standard error
fwrite($stderr, $error);

//close file pointer
fclose($stderr);
```

The code above opens the standard error file pointer, writes the error message to it, and then closes the file pointer. When executed, the error message will be displayed in the standard error output, which is usually the terminal window.

## Deep Dive

The `fwrite()` function is just one way to write to standard error in PHP. If you want to write multiple lines to standard error, you can use `file_put_contents()` instead, which will automatically open, write, and close the file pointer. Another option is to use the `error_log()` function, which allows you to specify the type of error (such as warning, notice, or fatal) and will automatically log the error to the server's error log file.

It's also worth noting that writing to standard error is different from writing to standard output. Standard error is used for error messages, while standard output is used for regular program output. Therefore, it's important to use the appropriate function depending on what you want to display.

## See Also

To learn more about handling errors and debugging in PHP, check out the following resources:

- [PHP Error Handling](https://www.php.net/manual/en/book.errorfunc.php)
- [PHP Debugging Tools](https://www.ionos.com/digitalguide/websites/web-development/debugging-tools-in-php/)
- [PHP Exception Handling](https://www.w3schools.com/php/php_exception.asp)