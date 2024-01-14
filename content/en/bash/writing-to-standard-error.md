---
title:                "Bash recipe: Writing to standard error"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why Writing to Standard Error is Important in Bash Programming

When working with Bash programming, it is essential to understand how to effectively write to standard error. This is because standard error is used for printing error messages and warnings, making it a crucial tool for debugging and troubleshooting code.

## How To Write to Standard Error in Bash

To write to standard error in Bash, we use the `>&2` operator to redirect the output to standard error. Let's look at an example of how this works:

```Bash
echo "This is a normal message"
echo "This is an error message" >&2
```

In the above code, the first `echo` statement will print to standard output, while the second `echo` statement will print to standard error. Now, let's see the output when executing this code:

```Bash
This is a normal message
This is an error message
```

As we can see, the error message is displayed differently, indicating that it is coming from standard error.

## Deep Dive into Writing to Standard Error

In Bash, standard error is represented by the file descriptor number 2, while standard output is represented by 1. This is why we use the `>&2` operator to redirect output from standard output to standard error.

Additionally, you can also use the `2>` operator to redirect only the error messages to a specific file. For example:

```Bash
grep "hello" not_existing_file 2> error.log
```

In the above code, any error messages from the `grep` command will be redirected to the `error.log` file instead of being displayed on the terminal.

It is also worth noting that standard error and standard output can be redirected separately, as shown in the example below:

```Bash
echo "This is a normal message" > output.log
echo "This is an error message" 2> error.log
```

In this case, the normal message will be redirected to the `output.log` file, while the error message will be redirected to the `error.log` file.

## See Also

- [Bash redirection operators](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
- [Understanding Standard Streams in Linux](https://www.digitalocean.com/community/tutorials/understanding-stdout-stderr-and-stdin-in-linux)

Writing to standard error in Bash may seem like a simple concept, but it is an essential skill for any Bash programmer. By understanding how to effectively use standard error, we can make our code more efficient and easily track and fix any errors that may occur.