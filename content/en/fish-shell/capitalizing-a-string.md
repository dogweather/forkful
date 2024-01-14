---
title:                "Fish Shell recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

As a programmer, you may come across situations where you need to manipulate strings, such as capitalizing them. This can be useful for various purposes, such as displaying names or titles in a proper format. In this blog post, we will explore how to capitalize a string using Fish Shell.

## How To

To begin, let's create a variable called "name" and assign it a string value:

```
set name "john doe"
```

Next, we can use the `string capitalize` command in Fish Shell to convert the first letter of each word in the string to uppercase. Let's see how it works by using the `echo` command to print the value of our "name" variable:

```
echo (string capitalize $name)
```

The output will be:

```
John Doe
```

As we can see, "john doe" has been converted to "John Doe" with the first letter of each word capitalized.

## Deep Dive

Now, let's take a deeper look at how the `string capitalize` command works. By default, it only capitalizes the first letter of each word. However, it also has an optional argument that allows us to specify which letters to capitalize. For example, if we only want to capitalize the first letter of the string, we can use the argument "1":

```
echo (string capitalize -1 $name)
```

The output will be:

```
John doe
```

Similarly, we can use other numbers as arguments to capitalize the first few letters of the string. For example, if we use "2", the first two letters will be capitalized:

```
echo (string capitalize -2 $name)
```

The output will be:

```
JOhn doe
```

We can also use the `string -r` command to reverse the order of the letters before capitalizing them. For example:

```
echo (string capitalize -r $name)
```

The output will be:

```
Doe john
```

This can be useful if we want to capitalize the last letter of a string instead of the first.

## See Also

- [Fish Shell string manipulation documentation](https://fishshell.com/docs/current/cmds/string.html)
- [Other useful Fish Shell commands](https://fishshell.com/docs/current/cmds.html)
- [Fish Shell tutorials and tips](https://fishshell.com/docs/current/tutorial.html)