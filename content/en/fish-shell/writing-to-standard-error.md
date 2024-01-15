---
title:                "Writing to standard error"
html_title:           "Fish Shell recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error, also known as stderr, is a common practice in programming. It allows for errors, warnings, and other important messages to be displayed to the user, separate from the regular output of a program. This ensures that the user can easily identify and address any issues that may arise.

## How To

To write to standard error in Fish Shell, we use the `echo` command with the `-e` flag and redirect the output to `stderr` using `>&2`. Here's an example:

```Fish Shell
echo -e "Error: Something went wrong" >&2
```

This command will print the error message "Something went wrong" to the standard error channel, designated as `2` in Fish Shell.

To further customize the message, we can use the `set_color` function to add colors and formatting. For example:

```Fish Shell
set_color red
echo -e "Error: Something went wrong" >&2
```

This will print the error message in red text.

## Deep Dive

In Fish Shell, stderr is just another output stream, along with stdout (standard output) and stdin (standard input). It can be redirected to a file, just like stdout, using the `>` and `>>` operators.

Additionally, Fish Shell allows for merging stderr and stdout into a single output stream using `&>`. This can be useful for storing both regular and error messages in the same logfile.

It's also worth noting that in Fish Shell, error messages are automatically displayed to the user, unlike other shells where they may need to be explicitly printed to the screen using `echo`.

## See Also

For more information about output streams in Fish Shell, check out the official documentation: 
- https://fishshell.com/docs/current/tutorial.html#redirecting-streams
- https://fishshell.com/docs/current/commands.html#echo