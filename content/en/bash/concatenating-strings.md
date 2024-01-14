---
title:                "Bash recipe: Concatenating strings"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why 
Have you ever needed to combine multiple strings together in your Bash script? Concatenating strings is a useful skill to have when working with text data or building dynamic outputs. 

## How To 
Using the `echo` command with the `-e` flag, we can combine strings by simply separating them with a space.
```Bash
echo -e "Hello" "World"
```
This will output `Hello World`. Another way to concatenate strings is by using the `+` operator. 
```Bash
str1="Hello"
str2="World"
echo $str1+$str2
```
This will output `Hello+World`. Note that the `+` operator does not add a space between the two strings. 
To add a space, we can use the `printf` command with the `%s` format specifier for strings and the `\n` escape sequence for a newline character. 
```Bash
printf "%s %s \n" $str1 $str2
```
This will output:
```
Hello World 
```
## Deep Dive
Behind the scenes, when we concatenate strings using the `-e` flag, Bash performs word splitting on the string arguments and prints them with spaces in between. For example, in the first `echo` command, Bash sees the two strings as separate arguments, and since the `echo` command automatically adds a space between arguments, we get the combined output of `Hello World`. 
When using the `+` operator, Bash simply performs string concatenation, resulting in `Hello+World`. To add a space, we need to use the `printf` command and specify the desired format.
Moreover, in Bash, we can also use variable assignment operators to add strings together. These operators include `+=` for concatenation, `~=` for pattern matching, and `=~` for regular expressions. 

Just like with any other programming language, it is important to keep in mind that when concatenating strings, we need to take into consideration the type and formatting of our strings to ensure the desired output. 

## See Also
- [Bash Guide for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Advanced Bash-Scripting Guide](http://linuxreviews.org/Bash_Guide_for_Beginners)
- [Bash String Manipulation](https://linuxhint.com/bash_string_manipulation/)

Concatenating strings in Bash is a simple and powerful tool that can enhance your scripting capabilities. With a few different methods to choose from, you can easily combine strings to create the dynamic outputs you need. Stay curious and keep exploring the world of Bash programming!