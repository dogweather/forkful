---
title:                "Fish Shell recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract a specific section of text from a longer string? Whether it's for data processing or formatting purposes, this is a common task that can be easily accomplished with substring extraction. In this blog post, we will explore how to use this feature in the Fish Shell, a powerful and user-friendly alternative to traditional command-line shells.

## How To

Let's start with a simple example. Imagine we have a string containing a list of names separated by commas:

```
Fish Shell

set names "John, Jane, Jack, Jill"
```

We want to extract only the first name from the list, in this case "John". With substring extraction, we can specify the starting and ending indices of the desired substring and use the `string` command with the `substr` option. In this case, we want the first name, starting from index 0 and ending at the first comma:

```
Fish Shell

set first_name (string substr -s 0 -e (string index ", " $names) $names)
```

The output of this command will be the substring "John", which we can then use for further processing. But what if we want to extract the last name instead? We can use the `index` command to find the index of the last comma in the string and then use that index as the ending value for `string substr`:

```
Fish Shell

set last_name (string substr -s (math (string length $first_name) + 2) -e (string index "," $names) $names)
```

This time, we use the `index` command with the `s` option to specify searching from the right to find the last comma. We also use the `math` command to add 2 to the length of the first name, accounting for the comma and space. The output will be the substring "Smith", the last name from our original string.

## Deep Dive

Now that we've covered the basics, let's dive deeper into how substring extraction works in Fish Shell. The `string` command has several options for specifying the starting and ending indices, such as using characters or patterns to mark the positions. Additionally, the `index` and `length` commands are helpful for finding the indices of specific characters or the length of a substring.

But what if we wanted to extract a substring based on a specific pattern rather than a fixed index? For this, we can use the `string match` command. Let's say we have a string containing a mixture of letters and numbers, and we want to extract only the numbers. We can use the `string match` command with a regular expression to find the pattern of digits in the string:

```
Fish Shell

set string "1a2b3c4d5e6f"
set numbers (string match -r "[0-9]+" $string)
```

After executing this command, the variable `numbers` will contain the substring "123456", which we can then use as needed.

## See Also

Now that you have a basic understanding of substring extraction in Fish Shell, here are some additional resources for further exploration:

- [Fish Shell documentation on `string` command](https://fishshell.com/docs/current/cmds/string.html)
- [Regular expressions in Fish Shell](https://fishshell.com/docs/current/tutorial.html#tutorial-regular-expressions)
- [Tips and tricks for efficient text manipulation in Fish Shell](https://fishshell.com/docs/current/tutorial.html#tutorial-advanced-tips)