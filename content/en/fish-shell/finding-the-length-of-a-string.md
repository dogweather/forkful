---
title:                "Fish Shell recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Do you ever need to find the length of a string in your Fish Shell programs? Whether you're coding a simple script or a more complex project, knowing the length of a string can be extremely useful. But how exactly do you do it?

## How To

Finding the length of a string in Fish Shell is actually quite straightforward. You can use the built-in `string` command with the `length` option to get the length of a given string. Here's an example:

```Fish Shell
set my_string "Hello, world!"
echo (string length $my_string)
```

The output of this code will be `13`, which is the length of the string "Hello, world!". You can also use the `echo` command to print the length directly without setting a variable.

```Fish Shell
echo (string length "This is a string")
```

The output in this case will be `16`, as there are 16 characters in the string "This is a string".

## Deep Dive

If you want to dive deeper into the `string` command, you can also use the `--bytes` option to get the byte length of a string instead of the character length. This is useful when dealing with non-English characters or emojis, as they may take up more than one byte.

You can also use the `--fields` option to get the number of fields in a string, split by a given delimiter. For example:

```Fish Shell
set my_string "Hello, world!"
echo (string fields "," $my_string)
```

The output will be `2`, as the string is split into two fields separated by a comma.

Similarly, the `--lines` option can be used to get the number of lines in a string, split by the newline character `\n`.

## See Also

For more information and examples on using the `string` command, you can refer to the official Fish Shell documentation or the following resources:

- [Fish Shell documentation on string](https://fishshell.com/docs/current/cmdsstring.html)
- [Fish Shell tutorial on strings](https://devopsheaven.com/fish-shell-strings/)
- [Fish Shell user documentation on string](https://fishshell.com/docs/current/user_guide.html#use-string)

Happy coding!