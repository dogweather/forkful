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

## Why

Writing to standard error in Bash can be useful when troubleshooting or debugging scripts. It allows you to see any error messages or warnings that your script may produce, which can help in identifying and fixing any issues.

## How To

To write to standard error in Bash, you can use the `echo` command followed by the `>&2` redirection operator. For example:

```Bash
echo "This is an error message" >&2
```

This will print the message to standard error instead of standard output. You can also use the `printf` command with the `%2>&1` format specifier to achieve the same result:

```Bash
printf "%2" "This is an error message"
```

To see the difference, you can try running the same command without the `>&2` or `%2>&1` redirection and see the message will be printed to standard output instead.

## Deep Dive

By default, any error messages or warnings produced by a command will be printed to standard error. However, some commands may also have an option to redirect errors to a different location. For example, the `grep` command has a `-q` option which suppresses all error messages.

Additionally, you can display both standard output and standard error using the `|&` operator, also known as "pipe and ampersand". This can be useful when you want to save all output, including errors, to a file. For example:

```Bash
ls -lR |& tee output.txt
```

This will list all files and directories recursively and save both standard output and standard error to the `output.txt` file.

## See Also

For more information on standard error and redirection in Bash, check out these resources:

- [Bash Redirections](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
- [Linux I/O Explained](https://www.computerhope.com/unix/uio.htm)
- [Understanding Shell Script's idiom: 2>&1](https://www.ibm.com/support/knowledgecenter/ssw_aix_71/osmanagement/2ampersand.htm)