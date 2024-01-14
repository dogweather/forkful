---
title:                "Bash recipe: Deleting characters matching a pattern"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Why Deleting Characters Matching a Pattern can be Useful

Have you ever encountered a situation where you needed to delete certain characters from a text file or string? Maybe you wanted to remove all the numbers from a string or delete specific symbols from a file. In such cases, using Bash to delete characters matching a pattern can be extremely useful.

By understanding how to delete characters matching a pattern in Bash, you can save time and effort in editing files or manipulating data. In this blog post, we will discuss how to achieve this task and provide a deeper understanding of the process.

## How To Use Bash to Delete Characters Matching a Pattern

To delete characters matching a pattern in Bash, we can use the `sed` and `tr` commands. Let's take a look at some coding examples to understand how to use these commands.

### Using `sed` command

The `sed` command, short for "stream editor," is used for manipulating text files. To delete characters matching a pattern with `sed`, we use the `s` (substitute) command followed by the pattern we want to delete and the replacement. Let's see how this works with an example.

Let's say we have a text file with a mix of lowercase and uppercase letters, and we want to delete all the uppercase letters. We can use the following command:

```Bash 
sed 's/[A-Z]//g' file.txt
```

Here, `s/[A-Z]//g` means we are substituting all uppercase letters with nothing, effectively deleting them. The `g` at the end means the substitution should be global, affecting all instances of the pattern.

The output of this command will be the contents of `file.txt` with all the uppercase letters removed.

### Using `tr` command

The `tr` command, short for "translate," is used for translating or deleting characters from standard input. To delete characters matching a pattern with `tr`, we use the `-d` option followed by the characters we want to delete. Let's see an example of this in action.

Let's say we have a string containing both letters and numbers, and we want to delete all the numbers. We can use the following command:

```Bash 
echo "a1b2c3d4" | tr -d '0-9'
```

Here, `-d '0-9'` means we are deleting all characters from `0` to `9`. The output of this command will be `abcd`, with all the numbers removed from the original string.

## Deep Dive into Deleting Characters Matching a Pattern

Now that we know how to use `sed` and `tr` to delete characters matching a pattern, let's understand a bit more about the process.

When using `sed` to delete characters, we can use a range of characters or a regular expression as a pattern. By using different ranges or expressions, we can tailor our deletion to specific needs. For example, we can delete only vowels or only punctuation marks from a file.

With `tr`, we can use the `-s` option to squeeze repeated characters. This can be useful if we want to delete all occurrences of a certain character, but not if they are repeated consecutively. For example, if we want to delete all spaces from a sentence but not the spaces between words, we can use `tr -s ' ' `.

It's also important to note that both `sed` and `tr` can also be used in conjunction with other commands, allowing for more complex and powerful manipulations of text.

## See Also

Here are some useful links for further learning:

- [sed tutorial from Linuxize](https://linuxize.com/post/sed-command-in-linux/)
- [tr command reference from The Linux Documentation Project](https://tldp.org/LDP/abs/html/string-manipulation.html#TRREF)
- [Regular Expressions tutorial from DigitalOcean](https://www.digitalocean.com/community/tutorials/using-grep-regular-expressions-to-search-for-text-patterns-in-linux)