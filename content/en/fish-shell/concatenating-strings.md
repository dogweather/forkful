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

## Why 
So, you're wondering why you would want to concatenate strings in a Fish Shell program? Well, let me tell you, it's a powerful tool for combining different pieces of text and creating more dynamic and flexible scripts. Plus, it can make your code more concise and easier to read.

## How To
Coding in Fish Shell is all about simplicity and efficiency, and concatenating strings is no different. To start, you'll need to use the `string join` command, which takes in multiple strings as arguments and joins them together with a specified delimiter. Here's an example:
```
Fish Shell
```
```
set cat = "cat"
set dog = "dog"
set animal = "These two amazing animals are: " (string join ", " $cat $dog)
echo $animal
```
```
These two amazing animals are: cat, dog
```
You can also use the `string concat` command, which works similarly to `string join` but doesn't require a delimiter. Here's an example:
```
Fish Shell
```
```
set name = "John"
string concat "Hello " $name ", welcome!"
```
```
Hello John, welcome!
```
And if you're dealing with more complex text, you can use the `string format` command to insert variables or values into a string. Here's an example:
```
Fish Shell
```
```
set fruit = "apple"
string format "My favorite fruit is %s." $fruit
```
```
My favorite fruit is apple.
```

## Deep Dive
Now, let's get into some deeper info about concatenating strings in Fish Shell. The `string join` and `string concat` commands both return the result as a new string, so you'll need to store it in a variable or use it in another command. Additionally, you can use the `string split` command to separate a string into different parts based on a delimiter. Here's an example:
```
Fish Shell
```
```
set fruits = "apple, banana, orange"
set fruits_arr = (string split ", " $fruits)
echo "I love " $fruits_arr[1] " and " $fruits_arr[3] "."
```
```
I love apple and orange.
```
Remember to use parentheses when defining variables for `string join` and `string concat`, as they are evaluated as commands and not regular assignments.

## See Also
- Official Fish Shell documentation for "string join": https://fishshell.com/docs/current/cmds/string-join.html
- Official Fish Shell documentation for "string concat": https://fishshell.com/docs/current/cmds/string-concat.html
- Official Fish Shell documentation for "string format": https://fishshell.com/docs/current/cmds/string-format.html
- Official Fish Shell documentation for "string split": https://fishshell.com/docs/current/cmds/string-split.html