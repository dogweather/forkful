---
title:                "Fish Shell recipe: Searching and replacing text"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to replace a specific word or phrase in a large text file? Perhaps you've made a typo multiple times throughout your code and need an efficient way to fix it. Whatever the reason may be, being able to quickly and easily search and replace text can save you a lot of time and frustration. With Fish Shell's built-in functionality, this task becomes a breeze.

## How To

To begin, open up your Fish Shell terminal and navigate to the directory that contains the file you want to modify. Let's say we have a file called "my_file.txt" that contains the phrase "Hello World!". We want to replace "Hello" with "Hi". To do this, we can use the "sed" command, which stands for "stream editor". The basic syntax is as follows:

```
fish shell
sed 's/{old_text}/{new_text}/g' < {file_name}
```

In our case, we would use the following command:

```
fish shell
sed 's/Hello/Hi/g' < my_file.txt
```

This will output the modified text to your terminal, but if you want to save the changes to the file itself, you can use the "-i" flag. This will modify the file in-place, so make sure to double check your changes before using this option.

```
fish shell
sed -i 's/Hello/Hi/g' my_file.txt
```

## Deep Dive

The "s" in the "sed" command stands for "substitute", while the "g" at the end means "global" (meaning it will replace all instances of the old text). You can also use regular expressions in your search and replace, making it even more powerful. For example, if we wanted to replace all instances of "Hello" or "Hi" with "Hey", we could use the following command:

```
fish shell
sed -i 's/Hello\|Hi/Hey/g' my_file.txt
```

This would replace both "Hello World!" and "Hi there!" with "Hey World!" and "Hey there!" respectively.

## See Also

- [Fish Shell Official Documentation](https://fishshell.com/docs/current/index.html)
- [Introduction to the Sed Command](https://www.gnu.org/software/sed/manual/sed.html)
- [Regular Expressions Tutorial](https://www.regular-expressions.info/tutorial.html)