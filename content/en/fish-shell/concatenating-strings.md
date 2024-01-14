---
title:                "Fish Shell recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why 
Concatenating strings may seem like a simple task, but it can be incredibly useful in programming. By combining different strings, you can create dynamic and customized outputs that can be used in various applications. Whether you are a beginner or an experienced programmer, understanding how to concatenate strings in Fish Shell can greatly enhance your coding abilities.

## How To
To concatenate strings in Fish Shell, we use the `string combine` command. This command takes two or more strings as arguments and combines them into a single string. Let's look at an example:

```
Fish Shell > set name "John"
Fish Shell > set greeting "Hello, "
Fish Shell > string combine $greeting $name
Hello, John
```

In this example, we have created two strings - `name` and `greeting` - and then used the `string combine` command to combine them into a personalized greeting. Notice how we used the `$` symbol before each string name to access their values. This is known as variable expansion and allows us to use variables as arguments in commands.

We can also concatenate strings without using variables. Take a look at this example:

```
Fish Shell > string combine "The " "sky " "is " "blue."
The sky is blue.
```

Here, we simply passed each string as an argument to the `string combine` command without using variables. We can also use more than two strings in the command, making it a versatile tool for string manipulation.

## Deep Dive
Under the hood, Fish Shell uses the `string join` function to concatenate strings. This function takes a delimiter as its first argument and joins the remaining arguments with that delimiter. The `string combine` command simply makes use of this function, making it easier to use for concatenation.

It's also worth noting that the `string combine` command can be used with any type of string, whether it's a single word or a phrase enclosed in quotes. This makes it a useful command for handling different types of strings in various scenarios.

## See Also
For more information on string concatenation in Fish Shell, check out the official [documentation](https://fishshell.com/docs/current/cmds/string_combine.html). You can also explore other useful Fish Shell commands in the [user guide](https://fishshell.com/docs/current/).