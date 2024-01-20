---
title:                "Writing to standard error"
html_title:           "Bash recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error in Bash refers to sending error messages or other important information to the standard error stream, known as "stderr." This is different from the standard output stream, or "stdout," which is used for regular program output. Programmers often use this method to differentiate between regular program output and error messages, making it easier to identify and troubleshoot problems.

## How to:

To write to standard error in Bash, you can use the "echo" command followed by the ">&2" redirect operator. For example:

```Bash
echo "Error: Something went wrong." >&2
```

This will print the error message to the standard error stream instead of the standard output stream. The "&" character is used to indicate that the following number is a file descriptor, and "2" refers to the standard error stream.

You can also use the "printf" command to format the error message before sending it to standard error. For example:

```Bash
printf "%s\n" "Error: File not found." >&2
```

This will send the formatted error message to standard error.

## Deep Dive:

Writing to standard error originated in the UNIX operating system, where standard streams were used for communication between programs. This concept was later adopted by other operating systems, including Linux. Alternative methods for writing to standard error include using the "error" or "errmsg" functions in C programming, or using the "LOG_ERR" level in the "syslog.h" header file.

In Bash, using the ">&2" redirect operator is the most common and recommended way to write to standard error. It is also possible to redirect both standard output and standard error to the same file by using the "2>&1" redirect operator.

## See Also:


Note: Keep in mind that writing to standard error can sometimes be used to suppress error messages, so use it wisely in your Bash scripts.