---
title:                "Writing to standard error"
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to standard error, `stderr`, outputs error messages separately from standard output, `stdout`. Programmers use `stderr` to report errors without messing with regular command outputs, which makes it easier to handle and log errors.

## How to:
```
# Redirect the echo command to standard error
echo "Error: Invalid input." >&2

# Using printf to write to standard error
printf "Error: File not found.\n" >&2

# Sample script that writes both to stdout and stderr
echo "Starting process..."
echo "Oops! Something went wrong." >&2
echo "Process complete."
```
Sample Output:
```
Starting process...
Process complete.
Oops! Something went wrong.
```
In the above, "Oops! Something went wrong." is sent to `stderr` and may appear out of order when mixed with `stdout` in a terminal since `stderr` is typically unbuffered.

## Deep Dive
Bash inherits the concept of 'file descriptors' from Unix, with `stdout` to fd `1` and `stderr` to fd `2`. Redirecting to `&2` sends the output to `stderr`. Historically, this separation allows for easier management and filtering, with `2>&1` being a common pattern to redirect `stderr` to `stdout`. A viable alternative to explicit redirection is to use `logger` for syslog integration or configuring the script to handle errors internally.

## See Also
- Bash Redirections Cheat Sheet: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Redirections
- Detailed overview of Bash scripting: https://www.tldp.org/LDP/Bash-Beginners-Guide/html/
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/