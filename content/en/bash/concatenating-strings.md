---
title:    "Bash recipe: Concatenating strings"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

##Why

Bash programming is a scripting language commonly used for automating tasks in Linux and other UNIX-based operating systems. One useful feature of Bash is the ability to concatenate, or combine, strings together. This can be helpful for creating dynamic and personalized scripts, as well as for creating more readable and organized code.

##How To

To concatenate strings in Bash, we can use the `printf` command with the `%s` format specifier. For example, let's say we have two strings: "Hello" and "World". We can concatenate them together using the following code:

```Bash
printf "%s%s" "Hello" "World"
```

The output of this code would be "HelloWorld". We can also add a space between the two strings by including a space within the format specifier:

```Bash
printf "%s %s" "Hello" "World"
```

The output would then be "Hello World". We can concatenate more than two strings by adding more `%s` format specifiers and corresponding input strings. 

We can also use variables to store our strings and then concatenate them together. For example:

```Bash
string1="Hello"
string2="World"
printf "%s %s" $string1 $string2
```

The output would be the same as the previous example. 

It's important to note that when using variables, we need to be careful of spacing. If there is a space within the variable, it will be included in the concatenated string. For example:

```Bash
string1="Hello "
string2="World"
printf "%s%s" $string1 $string2
```

The output would be "Hello World" with a space between the two words. 

##Deep Dive
In Bash, we can also use the `+` operator to concatenate strings. However, it's important to note that this method has some limitations. The main limitation is that we can only concatenate two strings at a time. For example:

```Bash
string1="Hello"
string2="World"
concatenated=$string1+$string2
echo $concatenated
```
The output would be "Hello+World". 

Another difference between using `+` and `printf` is that `+` will not automatically add a space between the concatenated strings. We would need to manually add a space between the two strings like this:

```Bash
concatenated=$string1" "$string2
```

##See Also
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/)

- [Bash String Manipulation](https://www.linuxjournal.com/content/bash-string-manipulation)

- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)