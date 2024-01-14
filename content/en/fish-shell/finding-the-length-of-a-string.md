---
title:    "Fish Shell recipe: Finding the length of a string"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to know the length of a string in your Fish Shell programming projects? Whether it's for manipulating data or for implementing certain logic, being able to find the length of a string can come in handy. In this blog post, we'll show you how to easily find the length of a string using Fish Shell.

## How To

To find the length of a string in Fish Shell, all you need is the `string length` command.

In the `Fish Shell`, type `string length` followed by the string you want to find the length of, within single quotation marks.

```
Fish Shell$ string length 'Hello World'
11
```

Here, we have used the `string length` command to find the length of the string "Hello World" and the output shows that the length is 11.

Similarly, you can find the length of any string by replacing "Hello World" with your desired string.

## Deep Dive

The `string length` command in Fish Shell is actually a built-in function that returns the length of a string in characters. It uses the `strlen` function from the C standard library to perform this task.

Keep in mind that when using the `string length` command, the characters within the string are counted, including spaces, special characters, and even the quotation marks. So the length of the string may not always be the same as the number of words in the string.

## See Also

- [Fish Shell's string length command documentation](https://fishshell.com/docs/current/commands.html#string-length)
- [Fish Shell tutorial video](https://www.youtube.com/watch?v=TG1BX5N9RWg)
- [Fish Shell user forums](https://fishshell.com/docs/current/index.html#discussions-and-support)