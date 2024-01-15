---
title:                "Searching and replacing text"
html_title:           "Gleam recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##Why

Have you ever found yourself with a huge block of text and needed to replace specific words or phrases within it? Maybe you're working on a website and want to update all instances of a certain product name, or you're writing a research paper and need to change the formatting of your citations. Whatever the case may be, using the Gleam programming language, you can quickly and easily search and replace text to make your life a whole lot easier. 

##How To

To search and replace text using Gleam, you'll first need to import the string module. Then, you can use the `replace` function to specify the string you want to search for, and what you want to replace it with. Let's say we have the following block of text and we want to replace all instances of "apple" with "orange": 

```
Gleam code is so much fun. I love eating apples while coding.
```

Using the `replace` function, our code would look like this:

```
Gleam string.replace("Gleam code is so much fun. I love eating apples while coding.", "apple", "orange")
```

And our output would be: 

```
Gleam code is so much fun. I love eating oranges while coding.
```

Simple enough, right? But what if we want to replace multiple words or phrases at once? Gleam's `replace` function also allows us to pass in a tuple of strings and their replacements, making it even easier to search and replace text. For example, if we want to replace "apple" with "orange" and "coding" with "relaxing", our code would look like this:

```
Gleam string.replace("Gleam code is so much fun. I love eating apples while coding.", [("apple", "orange"), ("coding", "relaxing")])
```

And our output would be: 

```
Gleam code is so much fun. I love eating oranges while relaxing.
```

##Deep Dive 

In addition to the `replace` function, Gleam also offers the `replace_n` function which allows you to specify the maximum number of replacements you want to make. This can be useful if you only want to replace the first few occurrences of a word or phrase. 

Gleam's `replace` and `replace_n` functions also allow for case-insensitive replacements, so you don't have to worry about variations in capitalization. And for more complex search and replace operations, Gleam's pattern matching capabilities can be used to perform even more specific replacements. 

##See Also

For more on the string module and its functions, check out the official Gleam documentation [here](https://gleam.run/documentation/stdlib/string/). And for further reading on pattern matching, take a look at this guide on functional patterns in Gleam [here](https://medium.com/@lpil/functional-pattern-matching-in-gleam-1674b3b0e6fe).