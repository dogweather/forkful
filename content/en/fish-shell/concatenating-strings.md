---
title:                "Fish Shell recipe: Concatenating strings"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Are you tired of manually combining strings in your Fish Shell programming? Well, fret not because concatenating strings is here to save the day! By combining different pieces of text, you can create more dynamic and versatile output in your Fish Shell scripts. In this blog post, we'll show you how to easily concatenate strings in Fish Shell and why it's such a game-changer.

## How To

Coding in Fish Shell is known for its simplicity and ease of use, and concatenating strings is no different. Here's how you can do it in just three easy steps:

1. First, let's define two strings that we want to concatenate:
```
set first "Hello"
set last "World!"
```
2. Next, we'll use the `string join` command to combine our strings and assign it to a new variable:
```
set concatenated (string join "" $first $last)
```
In this example, we used the empty string `""` as a separator, but you can use any character or text between the two strings. 

3. Lastly, to see the result of our concatenation, we'll use the `echo` command:
```
echo $concatenated
```
This will output "HelloWorld!", the combined result of our two strings. 

## Deep Dive

Now that you know how to concatenate strings in Fish Shell, let's take a deeper look into the different ways you can do it. Firstly, you can use the plus sign `+` to combine two strings, just like in many other programming languages:
```
set first "Fish "
set last "Shell"
set concatenated $first + $last
```
This will output "Fish Shell". 

Secondly, you can also use a `for` loop to concatenate multiple strings:
```
set fruits "apple" "banana" "orange"
set concatenated (for fruit in $fruits
    string join " / " $fruit
```

Lastly, you can even use `string match` to combine strings in a specific pattern. For example, to create a CSV row from a list of data:
```
set data "John" "Doe" "40"
set concatenated (string match "," $data)
```
This will output "John,Doe,40".

The possibilities are endless with concatenating strings in Fish Shell, and with some creativity, you can use it for all kinds of tasks.

## See Also

Curious to learn more about Fish Shell or want to enhance your Fish Shell skills? Check out these helpful resources:

- [Official Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell tutorial on YouTube](https://www.youtube.com/watch?v=JIX4oe5CXm0)
- [Fish Shell community forum](https://github.com/fish-shell/fish-shell/discussions)

Happy concatenating!