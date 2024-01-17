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

## What & Why?

Writing to standard error is a way for programmers to output error messages or other important information during the execution of their code. It allows them to communicate with the user or other programs about potential issues or important updates. This can help with debugging and make the code more user-friendly.

## How to:

To write to standard error in PHP, we use the `fwrite()` function with the `STDERR` file pointer. Here's an example:

```
<?php
$fp = fopen('php://stderr', 'w');
fwrite($fp, 'Something went wrong.'); 
fclose($fp);
```

The code above will write the string "Something went wrong." to the standard error output.

## Deep Dive:

Writing to standard error has been a common practice in programming for many years. It originated in Unix-like operating systems, where standard error is considered a separate stream from standard output. This feature allows programmers to differentiate between regular output and error messages, making it easier to identify and fix issues.

In PHP, there are two ways to write to standard error: using the `STDERR` file pointer or the `error_log()` function. Both methods achieve the same outcome, so it's up to personal preference which one to use.

It's essential to handle error messages properly in any programming language, and writing to standard error is just one way to do it. Other alternatives include writing to a log file or displaying error messages to the user directly. It's essential to choose the method that best fits the needs of your project.

## See Also:

- [PHP fwrite() Function](https://www.php.net/manual/en/function.fwrite.php)
- [Unix Standard Streams](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr))
- [PHP error_log() Function](https://www.php.net/manual/en/function.error-log.php)