---
title:    "Fish Shell recipe: Writing to standard error"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

If you're a developer or a frequent user of command line interfaces, you may have encountered the term "standard error" or "stderr" before. It may seem intimidating, but learning how to properly write to standard error can be extremely beneficial in troubleshooting and debugging your code.

## How To

Writing to standard error, also known as stderr, is a way to communicate error messages to users. It helps to differentiate between regular console output and error messages, making it easier to pinpoint issues in your code.

To write to stderr in the Fish Shell, you can use the `echo` command followed by the `-e` flag and the error message enclosed in quotation marks. For example:

```
Fish Shell> echo -e "This is an error message" >&2
```
The `>&2` at the end of the command tells the shell to output the message to standard error instead of the standard output. This ensures that the user sees the message as an error rather than regular output.

You can also use the `printf` command to write to stderr, which gives you more control over the formatting of the error message. For example:

```
Fish Shell> printf "%s\n" "Error: Something went wrong" >&2
```

You can also use conditionals to output different error messages based on certain conditions. For example:

```
Fish Shell> if [ $num -lt 0 ]; then echo -e "Number cannot be negative" >&2; fi
```

## Deep Dive

When writing to standard error, it's important to keep in mind that the messages will not be displayed in the usual console output. This means that users will need to actively check the standard error to view the messages.

Additionally, if you're writing to both standard output and standard error, the messages may not be displayed in the correct order. This is because stderr outputs immediately while stdout may be buffered before being displayed.

It's also worth noting that stderr can be redirected to a file, just like stdout. This can be useful for logging errors or capturing stderr output for later use.

## See Also
- [Fish Shell documentation on stderr](https://fishshell.com/docs/current/tutorial.html#tut_stderr)
- [Unix programming tutorial on stderr and stdout](https://www.gnu.org/software/libc/manual/html_node/Standard-Error.html)

Learning how to properly write to standard error can be a valuable skill for programming and troubleshooting. With the Fish Shell, it's easy to output error messages to stderr using simple commands. Keep in mind the nuances of stderr and how it differs from stdout, and you'll be well on your way to becoming a more efficient developer. Happy coding!