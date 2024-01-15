---
title:                "Writing to standard error"
html_title:           "PHP recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why
Writing to standard error is an essential tool in any PHP programmer's arsenal. It allows for better error handling and debugging in your code, making it easier to catch and fix any issues that may arise during execution.

## How To

To write to standard error in PHP, we use the `fwrite()` function. This function takes two parameters - the first being the stream we want to write to, and the second being the string we want to write.

Let's take a look at an example:

```PHP
<?php
$stream = fopen('php://stderr', 'w'); // open the standard error stream
fwrite($stream, 'An error has occurred!'); // write to the stream
fclose($stream); // close the stream
?>
```

The above code will write the string 'An error has occurred!' to the standard error stream, which can then be viewed in the command line or error log. This can be particularly useful when debugging code that is not producing the desired results.

Now, let's see what the output would look like in the command line:

```
php -f file.php
An error has occurred!
```

As we can see, our message is being displayed in the standard error stream.

## Deep Dive

When writing to standard error, it's important to understand how it differs from standard output. Standard error is used for error and diagnostic messages, while standard output is used for normal program output.

One thing to note is that standard error is unbuffered, meaning it will display any output immediately rather than waiting for the program to finish. It also has a higher priority, so any output to standard error will take precedence over standard output.

It's also worth mentioning that we can use `echo` to write to standard error, but it is not considered best practice. Using `fwrite()` allows for more control and flexibility in writing to the stream.

## See Also

Here are some helpful resources for further reading on writing to standard error in PHP:

- [PHP fwrite() Function](https://www.php.net/manual/en/function.fwrite.php)
- [Understanding Standard Streams in PHP](https://www.php.net/manual/en/features.commandline.io-streams.php)
- [PHP Output Buffering Explained](https://www.php.net/manual/en/features.output-buffering.php)