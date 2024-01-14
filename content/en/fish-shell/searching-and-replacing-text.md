---
title:                "Fish Shell recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Have you ever found yourself in a situation where you needed to quickly replace certain words or phrases in a file? Whether it's for formatting purposes or simply fixing a typo, text searching and replacing is a common need in programming. Luckily, the Fish Shell has a built-in feature that makes this task a breeze.

## How To
To search and replace text using Fish Shell, you can use the `sed` command. This command stands for "stream editor" and allows you to perform search and replace operations on files or streams of text. Let's take a look at an example.

```
Fish Shell code block:

# Create a sample file with some text
echo "Hello World, how are you?" > sample.txt

# View the file
cat sample.txt

# Search and replace using sed
sed -i 's/Hello/Hi/g' sample.txt

# View the updated file
cat sample.txt
```

Sample Output:
```
Hi World, how are you?
```

In this example, we used the `-i` flag to perform the search and replace operation directly on the file. The `s` in the command stands for "substitute", and we specified the word "Hello" to be replaced with "Hi" using the `g` flag (which stands for global, meaning replace all occurrences). You can also use regular expressions to specify more complex search patterns.

## Deep Dive
The `sed` command has various other options and flags that allow for more specific and advanced search and replace operations. For example, you can use the `-n` flag to suppress the default output and use the `p` command to print the replaced lines. You can also use the `-e` flag to specify multiple commands in one `sed` call. To learn more about all the available options, you can check out the Fish Shell documentation or any other online resources that cover this command in detail.

## See Also
- [Fish Shell documentation for `sed` command](https://fishshell.com/docs/current/cmds/sed.html)
- [Linuxize tutorial for `sed` command](https://linuxize.com/post/sed-command-in-linux/)
- [Video tutorial on search and replace in Fish Shell](https://www.youtube.com/watch?v=auct4bsbGu0)