---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Bash String Interpolation Unravelled

## What & Why?

String interpolation helps us insert or substitute variables directly into a string. Easy debugging and cleaner syntax are two perks you get with it.

## How to:

Here's a basic example, you got a name, you put it in a sentence.

```Bash
name="John Doe"
echo "Hello, ${name}!"
```
Output:

```Bash
Hello, John Doe!
```
Now let's play with variables in the string.

```Bash
age=27
echo "Hello, ${name}! You are ${age} years old."
```
Output:

```Bash
Hello, John Doe! You are 27 years old.
```

## Deep Dive

Historically, array referencing (`${array[1]}`) and string interpolation (`"Hello, ${name}!"`) became a thing in Bash 3.0 (early 2000's).

Alternative ways? Sure. `printf` functions, they handle string formatting cleanly.

```Bash
printf "Hello, %s! You are %d years old.\n" "$name" "$age"
```
Output:

```Bash
Hello, John Doe! You are 27 years old.
```

Under the hood, Bash does simple string replacement for interpolation, no biggie. However, beware of cases when variable expansion can go wrong, or quirks that come with `${}` syntax, such as `:-` or `:=` operators.

## See Also 

- [Bash 3.0 ChangeLog](https://tiswww.case.edu/php/chet/bash/NEWS) for official release notes.
- Explore more [`printf`](https://www.gnu.org/software/bash/manual/html_node/Printf-Examples.html) possibilities.
- Dive deeper into [`string manipulation`](https://www.tldp.org/LDP/abs/html/string-manipulation.html) in Bash.
- Check out the [`Parameter Expansion`](http://www.gnu.org/software/bash/manual/bash.html#Parameter-Expansion) in the Bash manual.