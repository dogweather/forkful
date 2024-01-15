---
title:                "Extracting substrings"
html_title:           "Bash recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why
Substring extraction is a useful programming technique in Bash that allows you to extract specific portions of text from a larger string. This can come in handy when dealing with large sets of data or when you need to manipulate text in a certain way.

## How To
To extract substrings in Bash, you can use the built-in `cut` command. Here's an example of extracting the first 5 characters from a string:

```Bash
myString="Hello World"
echo "${myString:0:5}"
```

The output of this code will be `Hello`, as it extracts the first 5 characters (starting at index 0) from the string. You can also specify a starting index and length for the substring, like this:

```Bash
echo "${myString:6:5}"
```

This will output `World`, as it extracts 5 characters starting from index 6. You can also extract from the end of the string by using negative indices:

```Bash
echo "${myString: -5}"
```
The output of this code will be `World`, as the negative index starts counting from the end of the string.

## Deep Dive
There are a few things to keep in mind when extracting substrings in Bash. First, Bash uses 0-based indexing, so the first character in a string is at index 0. This means that if you want to extract the first 5 characters, you would use an index of 0 and a length of 5.

Secondly, if you do not specify a length for the substring, Bash will extract all remaining characters from the specified index. For example, if you only specify an index of 6, Bash will extract all characters starting from index 6 until the end of the string.

You can also use variables for the indices and lengths of your substrings. This can come in handy when dealing with dynamic data. Just make sure to properly format the variables within the substring extraction syntax.

Lastly, you can use the `cut` command with a delimiter to extract substrings based on a specific character or pattern. This is useful when working with text separated by certain characters, such as commas or spaces.

## See Also
- [Bash Substring Extraction - Linuxize](https://linuxize.com/post/bash-substring/)
- [Cut Command in Bash - GeeksforGeeks](https://www.geeksforgeeks.org/cut-command-linux-examples/)
- [Bash Guide for Beginners - Linux Documentation Project](http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_10_02.html)