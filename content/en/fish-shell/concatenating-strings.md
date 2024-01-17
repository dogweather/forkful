---
title:                "Concatenating strings"
html_title:           "Fish Shell recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenating strings is the act of combining two or more strings together to create a new string. This is a common operation in programming as it allows for the creation of more complex and dynamic text outputs.

## How to:
Coding examples in Fish Shell:

```Fish Shell 
echo "Hello" "World" # Output: HelloWorld 

set first_name "John"
set last_name "Doe"

echo $first_name" "$last_name # Output: John Doe 
```

## Deep Dive:
Concatenating strings has been a fundamental operation in programming since the early days of computing. It enables programmers to manipulate and create dynamic text outputs, which is essential in many applications.

An alternative to concatenating strings is using string interpolation, which is a method of embedding variables directly into a string. However, this can become complex and challenging to read in longer strings.

In Fish Shell, concatenating strings is implemented using the `set` command, which allows for the creation and manipulation of variables. Variables can then be accessed by using the `$` symbol followed by the variable name.

## See Also:
- [Fish Shell Documentation on Variables](https://fishshell.com/docs/current/tutorial.html#variables)
- [Difference Between Concatenation and Interpolation in Programming](https://www.baeldung.com/cs/concatenation-vs-interpolation-programming)