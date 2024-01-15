---
title:                "Finding the length of a string"
html_title:           "Fish Shell recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to know the length of a string in your Fish Shell script? Maybe you need to validate user input or manipulate a string in some way. Whatever the reason, understanding how to find the length of a string is a useful skill to have in your Fish Shell programming arsenal.

## How To

To find the length of a string in Fish Shell, we can use the `string length` command. Let's take a look at an example:

```
Fish Shell $ set my_string "Hello world" 
Fish Shell $ echo (string length $my_string)
11
```

In this example, we have assigned the string "Hello world" to the variable `my_string` and then used the `string length` command to find its length. 

Another way to find the length of a string is by using the `string` command with the `length` option, like this:

```
Fish Shell $ set my_string "Hello world"
Fish Shell $ echo (string $my_string length)
11
```

Both of these methods will give you the same output, which is the length of the string.

It's also important to note that the `string length` command will also count spaces and special characters. So if we modified our example string to include special characters, like this:

```
Fish Shell $ set my_string "Hello$!@#world"
```

The output of the `string length` command would be 16, while the `string length` option would still give us a length of 11.

## Deep Dive

Now, let's take a deeper look at how the `string length` command works. The `length` function in Fish Shell actually uses the `wc` (word count) command, which counts the number of characters, words, and lines in a given input. 

When we use the `string length` command, we are essentially telling Fish Shell to count the number of characters in the string, which is why it can also include spaces and special characters.

## See Also

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell Beginner's Guide](https://www.codementor.io/@arpitbhayani/an-in-depth-guide-to-fish-shell-8sj6b7cgg)
- [Fish Shell Tutorials](https://www.linux.com/training-tutorials/beginners-guide-fish-shell/)