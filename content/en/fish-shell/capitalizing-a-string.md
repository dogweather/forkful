---
title:    "Fish Shell recipe: Capitalizing a string"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why 

Have you ever needed to capitalize a string in your Fish Shell programming? Perhaps you want to make the first letter of a name or sentence uppercase, or maybe you need to follow strict capitalization rules for a project. Whatever the reason, using Fish Shell's built-in function can quickly and easily transform your text.

## How To 
We'll start by creating a simple string variable with the text we want to capitalize:

```
fish
set my_string "hello world"
```
Next, we'll use the `string capitalize` function to make the first letter of our string uppercase:

```
fish
set my_string (string capitalize $my_string)
```
Let's see the result by printing our variable to the terminal:

```
fish
echo $my_string
Hello world
```
As you can see, our string now starts with a capital "H" instead of a lowercase "h". But what if we want to capitalize every word in our string? We can use a combination of the `string capitalize` and `string join` functions:

```
fish
set my_string "this is a sentence"
set words (string split " " $my_string)
set capitalized_words (string capitalize $words)
set capitalized_string (string join " " $capitalized_words)
echo $capitalized_string
This Is A Sentence
```
In this example, we split our string into individual words, capitalized each word, and then joined them back together to create a new string with all words capitalized.

## Deep Dive 
The `string capitalize` function uses the `strtoupper()` C function to capitalize the first character of a string. However, this function only works for ASCII characters, so it may not work as expected for non-English strings. In these cases, it's best to use a third-party library like `capitalize` or `string.macro` to handle Unicode characters.

It's also worth noting that the `string capitalize` function will not change any characters other than the first one to uppercase. So if your string includes numbers, symbols, or already capitalized words, they will remain unchanged.

## See Also 
- [Fish Shell documentation on string manipulation](https://fishshell.com/docs/current/commands.html#string-manipulation)
- [Fish Shell tutorial on strings and variables](https://fishshell.com/docs/current/tutorial.html#tutorial-string-variables)
- [GNU Fish reference manual for more advanced string techniques](https://fishshell.com/docs/current/index.html#Reference-manual)

Thank you for reading! Happy coding with Fish Shell!