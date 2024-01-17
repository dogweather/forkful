---
title:                "Deleting characters matching a pattern"
html_title:           "Bash recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern in Bash is a way for programmers to quickly and efficiently manipulate text in their scripts. This can be useful for various tasks, such as removing unwanted characters or formatting data in a specific way.

## How to:

To delete characters matching a pattern in Bash, you can use the `sed` command with the `s` option and specify the pattern you want to match. Here's an example:

```Bash
# Say we have a file named "data.txt" with the following contents:
# Hello, my name is John!
# My favorite color is blue.

# We want to remove all instances of the letter "o" in the file, so we use the following command:
sed 's/o//g' data.txt

# This will output:
Hell, my name is Jhn!
My favrite clor is blue.
```

## Deep Dive

There are a few different ways to manipulate text in Bash, such as using `awk` or `grep`. However, `sed` is specifically designed for substituting or deleting text.

The `s` option in `sed` stands for "substitute" and allows you to specify a pattern to match and what you want to replace it with. In our example, we used an empty string as the replacement, which effectively deleted any instances of the pattern we specified.

You can also use regular expressions to match more complex patterns and delete multiple characters at once. For example, if we wanted to remove all digits from a string, we could use the following command:

```Bash
# Say we have a string with random numbers in it, such as "abc123def456ghi"
echo "abc123def456ghi" | sed 's/[0-9]//g'

# This will output:
abcdefghi
```

## See Also

To learn more about `sed` and how it works, you can refer to the official documentation: https://www.gnu.org/software/sed/manual/sed.html

You can also explore other methods for text manipulation in Bash, such as using `awk` or `grep`:
- https://www.gnu.org/software/gawk/manual/gawk.html
- https://www.gnu.org/software/grep/manual/grep.html