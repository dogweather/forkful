---
title:    "Fish Shell recipe: Concatenating strings"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why

Concatenating strings is a common task in programming, especially when it comes to manipulating text data. Whether you are working on a simple script or a complex application, the ability to combine strings can be incredibly useful. In this post, we will explore how to do so using Fish Shell, a user-friendly and powerful command-line shell.

## How To

Fish Shell contains several built-in functions that make string concatenation a breeze. Let's take a look at a simple example where we want to combine two strings, "Hello" and "World" into one:

```Fish Shell
echo "Hello" "World"
```

The output of this command would be:

```Fish Shell
Hello World
```

As you can see, the strings were concatenated without any extra effort. But what if we want to add a space between the two words? We can use the `string join` function to achieve this:

```Fish Shell
echo (string join " " "Hello" "World")
```

The output would now be:

```Fish Shell
Hello World
```

Notice how we used the `( )` parenthesis to wrap the `string join` function, this is necessary to ensure the function is executed before the `echo` command. Another helpful function for concatenating strings is `string format`. This function allows us to insert variables or expressions into our string in a specific order. Here's an example:

```Fish Shell
set name "John"
set age 25
echo (string format "My name is %s and I am %d years old." $name $age)
```

The output would be:

```Fish Shell
My name is John and I am 25 years old.
```

## Deep Dive

Behind the scenes, Fish Shell uses the `string cat` function to perform string concatenation. This function takes multiple strings as arguments and combines them into one. The `string join` and `string format` functions are just convenient wrappers around `string cat`. It's worth noting that Fish Shell also supports variable expansion, so you can concatenate strings by simply joining variables together without using any special functions.

## See Also

- [Fish Shell documentation on string manipulation](https://fishshell.com/docs/current/cmds/string.html)
- [Tutorial on using Fish Shell functions](https://medium.com/swlh/how-to-use-fish-shell-functions-to-boost-your-cli-productivity-cf0c6f8fd858)
- [Official Fish Shell website](https://fishshell.com/)