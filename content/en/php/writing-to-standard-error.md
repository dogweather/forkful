---
title:    "PHP recipe: Writing to standard error"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why Writing to Standard Error is Useful

As a programmer, it's important to have a way to communicate important information or errors to the user. This is where writing to standard error comes in. It allows for the output of error messages or debugging information separate from the regular output, making it easier for the user to identify and address any issues. 

## How To Write to Standard Error in PHP

Writing to standard error in PHP is a simple process that can be done in just a couple of lines of code. First, we need to open the stderr stream using the `fopen()` function with the mode "w" for writing. Next, we can use the `fwrite()` function to write whatever message or information we want to output. Finally, we need to close the stream using the `fclose()` function. 

Let's take a look at an example:

```PHP
$stderr = fopen('php://stderr','w');
fwrite($stderr, "This is an error message.");
fclose($stderr);
```

Running this code will output "This is an error message" to the standard error stream, which can be viewed in the terminal or error log.

## Deep Dive into Writing to Standard Error

While writing to standard error may seem like a simple concept, there are a few things to keep in mind when using it in your code. 

One important consideration is the difference between standard error and standard output. Standard output, known as `stdout`, is used for regular output of information, while standard error, known as `stderr`, is used for error messages or debugging information. It's important to use the correct stream to avoid confusion for the user.

Another thing to keep in mind is that writing to standard error will not stop the execution of the code. This means that even if there is an error or message written to standard error, the code will continue to run unless specific checks are put in place.

In some cases, you may also want to redirect the standard error output to a file instead of the terminal. This can be done by using the `php -d display_errors=stderr script.php > error.log` command, which will redirect any errors to the "error.log" file instead of displaying them in the terminal.

## See Also

- [PHP Error Handling - Writing to Standard Error](https://www.php.net/manual/en/ref.errorfunc.php)
- [PHP Streams - Writing to Standard Error](https://www.php.net/manual/en/wrappers.php)
- [Tutorial: PHP Error Logging](https://www.w3schools.com/php/php_error.asp)