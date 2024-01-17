---
title:                "Searching and replacing text"
html_title:           "Bash recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is a process of finding specific words or phrases in a piece of text and replacing them with desired ones. Programmers use this technique to quickly make changes to their code without manually going through every line. It's an efficient way to make multiple changes at once, saving time and effort.

## How to:

Here's an example of searching and replacing text in Bash:

```Bash
# Search for word "Hello" and replace it with "Hi"
sed -i 's/Hello/Hi/g' example.txt
```

The `-i` flag is used to make the changes in the file itself. Without it, the changed text will only be displayed in the output. Similarly, we can use the `grep` command to search for specific words in a file:

```Bash
# Search for lines containing word "apple" in example.txt
grep "apple" example.txt
```

This will return all the lines in the file that contain the word "apple". We can also use the `find` command to search for files with specific names or extensions:

```Bash
# Find all files with ".txt" extension in current directory
find . -name "*.txt"
```

## Deep Dive:

Searching and replacing text has been a common practice since the early days of computing. It was first introduced in the `ed` editor in the 1960s, and later popularized in `sed`, which stands for "stream editor". Nowadays, there are many tools and scripting languages that provide powerful search and replace capabilities, such as Perl, Python, and even plain old command line utilities like `awk`.

One alternative to using command line tools is integrating text search and replace functionality into your text editor. Many modern editors come with this feature built-in or offer plugins that can do it. This can be especially useful when making changes to large projects with multiple files.

In terms of implementation, text searching and replacing is usually done using regular expressions, also known as "regex". Regular expressions define a pattern that the search phrase must match, making the search more flexible and accurate.

## See Also:

- [ed: The Standard Text Editor](https://www.gnu.org/fun/jokes/basics.plus-joke)
- [sed: Unix Sed utility](https://www.gnu.org/software/sed/)
- [Regular Expressions in Bash](https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html)