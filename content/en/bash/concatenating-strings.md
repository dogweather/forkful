---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

# Bash Scripting: String Concatenation

## What & Why?

String concatenation is the process of appending, or 'gluing,' one string to the end of another. This handy trick is used frequently to create custom messages, filenames, or commands.

## How to:

Here's a couple of ways to concatenate strings in Bash.

```Bash
# Method 1: Direct joining
string1="Hello, "
string2="world!"
greeting=$string1$string2
echo $greeting
```

The output will look like this:

```
Hello, world!
```

Or, you can use the "+=" operator for in place concatenation, like so:

```Bash
# Method 2: In-place joining
string1="Hello, "
string1+="world!"
echo $string1
```

The output is:

```
Hello, world!
```

## Deep Dive

Bash, like other UNIX shells, doesn't really have a dedicated concatenation operation. It's all about just gluing strings together, either by using variable expansion or the "+=" operator. 

The "+=" method, added in Bash version 3.1, adds or concatenates right-sided string to the left-sided operand.

Alternatives to Bash include programming languages like Python or Perl. They offer more sophisticated string manipulation functions, but for simplicity and speed – Bash is supreme!

## See Also

1. [GNU Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
2. [Bash Guide for Beginners by Machtelt Garrels](http://www.tldp.org/LDP/Bash-Beginners-Guide/html/)
3. [Advanced Bash-Scripting Guide by Mendel Cooper](http://www.tldp.org/LDP/abs/html/abs-guide.html)  
4. [Learn Shell Programming](https://www.learnshell.org/)