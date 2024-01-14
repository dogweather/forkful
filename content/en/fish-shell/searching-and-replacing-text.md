---
title:    "Fish Shell recipe: Searching and replacing text"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Search and replace is a common task in programming, especially when working with large amounts of text. By using the Fish Shell, you can easily perform these actions with just a few simple commands. This will save you time and effort, allowing you to focus on other aspects of your work.

## How To

Coding examples and sample output within "```Fish Shell ...```" code blocks.

#### Search for a specific word and replace it with another

```Fish Shell
# The 'sed' command is used for search and replace
sed 's/old_word/new_word/g' file_name.txt
```
*file_name.txt* is the name of the file you want to perform the search and replace on. Replace *old_word* with the word you want to replace and *new_word* with the word you want to use in its place. The `g` at the end means it will perform the replacement for every instance of the word in the file. 

#### Search and replace with regular expressions

```Fish Shell
# Replace all numbers with 'X'
sed 's/[0-9]/X/g' file_name.txt
```

In this example, we use regular expressions to replace all numbers in the file with the letter 'X'. The bracket notation `[0-9]` signifies any number from 0-9 and the `g` at the end performs the replacement for every instance.

## Deep Dive

When using the `sed` command for search and replace, it is important to understand the different flags that can be used to modify the behavior.

- `s`: This flag indicates that we are searching and replacing.
- `g`: This flag means that the replacement will be performed for every instance of the search word or pattern.
- `i`: This flag makes the search case insensitive.
- `p`: This flag prints out the lines where the search and replace was successful.
- `w`: This flag only performs the replacement on lines that match the exact search word.

You can also use combinations of these flags to achieve more specific search and replace results.

## See Also

- [Fish Shell: The friendly interactive shell](https://fishshell.com/)
- [Sed: Stream editor for filtering and transforming text](https://www.gnu.org/software/sed/)
- [Regular Expressions in Fish Shell](http://fishshell.com/docs/current/index.html#regex)