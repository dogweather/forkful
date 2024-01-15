---
title:                "Printing debug output"
html_title:           "Fish Shell recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
Debug output, also known as logging, is an essential tool for any programmer. It allows you to track the execution of your code and identify any errors or unexpected behavior. Printing debug output in a clear and organized manner can greatly improve the debugging process and save you time in the long run.

## How To
To print debug output in Fish Shell, you can use the `echo` command followed by the `-d` option, which stands for "debug". For example:

```
Fish Shell

echo -d "Debug output"
```

This will print the string "Debug output" along with the line number and file name where the `echo` command was executed.

You can also use the `set -q` command in combination with a variable to check if a certain condition is met, and then print the debug output. For example:

```
Fish Shell

set var "Debug variable"

if set -q var
    echo -d $var
end
```

This will only print the debug output if the condition `set -q var` is true.

## Deep Dive
By default, Fish Shell will print debug output to the standard error stream, which is usually displayed in red. However, you can also customize the output color using the `-c` option followed by a color name or RGB value. For example:

```
Fish Shell

echo -d -c red "Red debug output"
```

This will print "Red debug output" in red to the standard error stream.

Additionally, you can use the `-f` and `-l` options to print the file name and line number respectively, without printing any actual debug output. This can be useful when you want to track the execution of your code without cluttering the output with unnecessary messages.

## See Also
- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Debugging in Fish Shell](https://fishshell.com/docs/current/commands.html#debug-output)