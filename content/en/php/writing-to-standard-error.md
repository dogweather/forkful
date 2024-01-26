---
title:                "Writing to standard error"
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (`stderr`) means outputting error messages and diagnostics separate from standard output (`stdout`). Programmers do it to debug code and provide error information without mixing it with regular program output.

## How to:

You can write to `stderr` in PHP with `fwrite()` or stream wrappers. Here's how:

```PHP
<?php
// Writing to stderr with fwrite
fwrite(STDERR, "This is an error message.\n");

// Using a stream wrapper
file_put_contents('php://stderr', "This is another error message.\n");
?>
```

Sample output (in the console):
```
This is an error message.
This is another error message.
```

## Deep Dive

Historically, separating `stdout` and `stderr` comes from Unix's way of handling I/O streams. Other languages like C have similar conventions. Alternatives in PHP could involve logging libraries or custom error handlers, but writing directly to `stderr` is straightforward for console applications. Behind the scenes, `stderr` is an unbuffered output stream, which means messages are immediately flushed without waiting.

## See Also

- PHP Manual on Predefined Constants (STDERR): https://www.php.net/manual/en/features.commandline.io-streams.php
- PHP Manual on error handling functions: https://www.php.net/manual/en/book.errorfunc.php
- Wikipedia on Standard streams: https://en.wikipedia.org/wiki/Standard_streams
