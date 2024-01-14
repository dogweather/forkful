---
title:                "Fish Shell recipe: Finding the length of a string"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

As a Fish Shell programmer, you may often need to manipulate strings in your code. One of the basic operations is finding the length of a string, which can be useful for tasks such as validating input or formatting output. In this blog post, we will guide you through the process of finding the length of a string in Fish Shell.

## How To

To find the length of a string in Fish Shell, we will use the built-in `strlen` command. This command takes a string as input and returns the number of characters in the string. Let's take a look at an example:

```Fish Shell
set str "Hello, world!"
strlen $str
```

The output of this code block will be `13`, indicating that the string "Hello, world!" has 13 characters.

You can also use the `string length` function to find the length of a string. Here's an example:

```Fish Shell
string length "Fish Shell"
```

The output will be `10`, as the string "Fish Shell" has 10 characters.

## Deep Dive

Behind the scenes, the `strlen` command and `string length` function use different methods to find the length of a string. The `strlen` command uses the `bw` built-in, which is a bytecode interpreter that runs faster than regular Fish Shell code. On the other hand, the `string length` function uses a loop to iterate through the string and count the characters.

It is also worth mentioning that the `string length` function allows you to specify a character set to consider when counting the length of a string. This can be useful when working with international or special characters.

## See Also

- [Fish Shell documentation on `strlen`](https://fishshell.com/docs/current/cmds/strlen.html)
- [Fish Shell documentation on `string length`](https://fishshell.com/docs/current/cmds/string-length.html)

Now that you know how to find the length of a string in Fish Shell, you can use this knowledge to make your code more robust and efficient. Happy coding!