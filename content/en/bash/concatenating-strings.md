---
title:                "Bash recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is a common task in Bash programming, and is used to combine multiple strings into a single, longer string. This can be useful for tasks such as creating dynamic file names, constructing URLs, or displaying information to the user.

## How To

To concatenate strings in Bash, we use the `printf` command, which allows us to format and print strings in a variety of ways. Let's look at some examples to see how it works.

```
Bash code: printf "Hello %s, welcome to my blog!" "readers"
Output: Hello readers, welcome to my blog!
```

In this example, we are using `printf` to output the string "Hello readers, welcome to my blog!" by combining the string "Hello" with the string "readers" using the `%s` formatting specifier.

We can also concatenate multiple strings together by using multiple `%s` specifiers and providing a corresponding string for each one.

```
Bash code: printf "%s is learning %s programming." "I" "Bash"
Output: I am learning Bash programming.
```

In addition to using `printf`, we can also use the `+=` operator to concatenate strings. This is often used within a loop or function to add strings together and store them in a variable.

```
Bash code: greeting="Welcome "
    greeting+="to my blog!"
    echo $greeting
Output: Welcome to my blog!
```

## Deep Dive

When concatenating strings, it is important to pay attention to whitespace and other characters, as they can affect the output. For example, if we use `+=` without adding a space between the two strings, it will result in a single word without a space in between.

Additionally, we can use `printf` to add formatting elements such as new lines and tabs to our concatenated strings. This can be helpful when creating more complex and visually appealing outputs.

```
Bash code: printf "Hello %s,\n\tWelcome to my blog!" "readers"
Output: Hello readers,
        Welcome to my blog!
```

It is also worth mentioning that we can concatenate not only strings, but also variables, numbers, and other data types. This makes it a versatile tool for building dynamic outputs in our Bash scripts.

## See Also

- [Bash Reference Manual: printf](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)
- [Unix & Linux Stack Exchange: How to concatenate strings in Bash?](https://unix.stackexchange.com/questions/71614/how-to-concatenate-two-strings-together-in-sh)
- [Bash Beginners Guide: Concatenating](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_09_02.html)