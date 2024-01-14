---
title:    "Bash recipe: Concatenating strings"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to combine multiple strings together in your Bash program? Maybe you wanted to create a dynamic file name or display a customized message to the user. Whatever the reason, string concatenation is a useful skill to have in your Bash programming toolkit.

## How To

The syntax for concatenating strings in Bash is fairly simple. To combine two strings, you simply need to write them next to each other without any special characters in between. Let's take a look at an example:

```Bash
str1="Hello"
str2="world"
echo $str1$str2
```

The output of this code would be:

```
Helloworld
```

You can also add a space between the strings by adding a space character in between them. Let's see how that looks:

```Bash
str1="Hello"
str2="world"
echo $str1" "$str2
```

The output of this code would be:

```
Hello world
```

## Deep Dive

While the syntax for string concatenation in Bash is fairly straightforward, there are a few things to keep in mind. First, make sure to leave a space between the strings if you want a space in between them when they are combined. Otherwise, the strings will be merged together without any separation.

You can also concatenate more than two strings at a time. Simply write them next to each other with spaces in between. For example:

```Bash
str1="Hello"
str2="my"
str3="friend"
echo $str1" "$str2" "$str3
```

The output of this code would be:

```
Hello my friend
```

Another important thing to note is that you can use variables in string concatenation. This allows you to combine static and dynamic elements to create more complex strings. Here's an example:

```Bash
greeting="Hello"
name="John"
echo $greeting" "$name", how are you?"
```

The output of this code would be:

```
Hello John, how are you?
```

## See Also

For more information on string concatenation in Bash, check out the following resources:

- [Bash String Manipulation](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
- [Bash String Concatenation](https://linuxize.com/post/bash-concatenate-strings/)
- [Bash Cookbook: String Operations](https://bashcookbook.com/bashinfo/source/bash-4.0/examples/scripts/misc/concatop)

Now you have all the knowledge you need to start concatenating strings in your Bash programs. Happy coding!