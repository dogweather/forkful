---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Welcome to the Fish of String Concatenation!

## What & Why?

Concatenating strings is a way of joining two or more text strings end-to-end. Programmers do this to manipulate, format, and display data effectively.

## How to:

In Fish Shell, you concatenate strings by using a command and two strings. Here's an easy cheat sheet for it:

```Fish Shell
# Set two strings
set string1 "Hello"
set string2 "World"

# Concatenate the strings
echo $string1$string2
```

When run, this will output:

```Fish Shell
HelloWorld
```

If you want to add a space between the strings, simply include the space within one of the strings like this:

```Fish Shell
# Include space in the first string
set string1 "Hello "
set string2 "World"

# Perform string concatenation
echo $string1$string2
```

This give the output:

```Fish Shell
Hello World
```

## Deep Dive:

While string concatenation in Fish Shell is relatively straightforward, it's worth knowing a few additional things. Fish Shell, like most UNIX shells, doesn't distinguish string variables from other types, so no special syntax is required for string concatenation compared to the likes of C or Perl.

Alternatives to the `set` command used above do exist. For instance, using `printf` to concatenate strings:

```Fish Shell
printf "%s%s\n" $string1 $string2
```

While Fish Shell doesn’t require explicit declaration of variable types like some other languages do, keep in mind that Fish is case-sensitive. So, `$String1` and `$string1` would be treated as different variables.

### See Also:

- Official Fish Shell Documentation: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Quick Fish Scripting Guide: [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
- In-depth look at UNIX Shell string manipulation: [https://bash.cyberciti.biz/guide/Cut,_copy,_and_paste](https://bash.cyberciti.biz/guide/Cut,_copy,_and_paste)

Dive into these resources for more wealth on string concatenation and other stellar tricks in the Fish Shell universe!