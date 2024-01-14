---
title:                "Fish Shell recipe: Writing to standard error"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

As a Fish Shell programmer, you may wonder why you would want to write to standard error. Writing to standard error allows you to display error messages or other relevant information to the user, which can be helpful for debugging and troubleshooting your code.

## How To

To write to standard error in Fish Shell, you can use the `echo` command with the `-e` and `-o` options. The `-e` option enables interpretation of backslash escapes, while the `-o` option specifies the output destination as standard error. Here's an example:

```Fish Shell
echo -e "This is an error message" >&2
```

The `>&2` at the end redirects the output to standard error, as indicated by the `2` after the `&` symbol. This will print the message "This is an error message" to the standard error stream.

You can also write to standard error using the `printf` command. Here's an example:

```Fish Shell
printf "%s\n" "This is an error message" >&2
```

This will produce the same output as the `echo` command. However, `printf` provides more formatting options, such as specifying the data type and using placeholders for variables.

## Deep Dive

In Fish Shell, standard error is represented by the file descriptor `2`. This file descriptor is used to handle any error messages or other output that should not be sent to the standard output. By default, both standard error and standard output are displayed in the terminal, but you can redirect them to different locations if needed.

For example, you can redirect standard error to a file for later review:

```Fish Shell
echo "This is an error message" >> error.log
```

Or you can use a pipe to redirect standard error to the standard input of another command:

```Fish Shell
ls -l | grep "file" 2>&1
```

In this case, the `2>&1` redirects the standard error to the same destination as the standard output, which is the `grep` command. This can be useful for filtering out specific errors or displaying them in a different context.

## See Also
- [Writing to Standard Error in Bash](https://www.shell-tips.com/bash/write-to-stderr/)
- [Fish Shell Documentation - Standard Error](https://fishshell.com/docs/current/tutorial.html#standard-error)
- [Understanding Standard Input, Output, and Error](https://www.howtogeek.com/435903/what-are-stdin-stdout-and-stderr-on-linux/)