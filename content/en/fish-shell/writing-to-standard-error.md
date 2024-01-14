---
title:    "Fish Shell recipe: Writing to standard error"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why

If you're new to Fish Shell, one of the first things you may notice is the ability to write to standard error using the `echo` command. But why would you want to do that? Writing to standard error allows you to print out error messages or warnings separately from regular output, making it easier to spot and troubleshoot any issues in your code.

## How To

To write to standard error in Fish Shell, you can use the `echo` command with the `-e` flag and `>&2` at the end. Let's take a look at an example:

```
Fish Shell
$ echo -e "\e[31mError:\e[0m Something went wrong" >&2
```

The `\e[31m` and `\e[0m` are escape sequences that change the color of the text to red and then reset it back to the default color. The `>&2` redirects the output to standard error, allowing us to see the error message separately from regular output.

The output of the above command would look like this:

```
Error: Something went wrong
```

Notice how the error message is printed in red, making it stand out from the regular output.

## Deep Dive

Writing to standard error is useful when you want to differentiate between regular output and error messages, but there is more to it than just changing the color of the text. You can also use it to redirect error messages to a separate file for easier debugging.

For example, let's say you have a shell script that runs some commands and you want to save any error messages to a log file. You can do that by using `2>>` to redirect standard error to a file. Here's an example:

```
Fish Shell
$ ./script.sh 2>> error_log.txt
```

This will run the commands in the script and any error messages will be saved in the `error_log.txt` file instead of being displayed in the terminal.

## See Also

For more information on writing to standard error in Fish Shell, you can check out the following resources:

- [Fish Shell tutorial](https://fishshell.com/docs/current/tutorial.html)
- [Redirections in Fish shell](https://fishshell.com/docs/current/tutorial.html#tutorial-setup-prompt)
- [How to format text in Fish Shell](https://fishshell.com/docs/current/#formatting-text)

Happy coding!