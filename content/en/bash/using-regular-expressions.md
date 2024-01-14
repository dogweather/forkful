---
title:                "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are powerful tools used for pattern matching and text manipulation in programming. By understanding and utilizing regular expressions, you can streamline common tasks and make your code more efficient. 

## How To

To use regular expressions in Bash, you first need to make sure you have the `grep` command installed. This command allows you to search for patterns within text. Here's a basic example using a simple regular expression:

```Bash
# Search for the word "hello" in a file named example.txt
grep "hello" example.txt
```

You can also use regular expressions to match more complex patterns, such as specific characters, numbers, or words. Let's say you want to find all instances of phone numbers in a text file. You can use the regular expression `\d{3}-\d{3}-\d{4}` to match the common phone number format of 123-456-7890. Here's an example command:

```Bash
# Search for phone numbers in a file named contacts.txt
grep "\d{3}-\d{3}-\d{4}" contacts.txt
```

You can even use regular expressions in conditional statements to create more advanced logic in your code. Here's an example of finding all files with "report" in the name:

```Bash
# Loop through all files in a directory and print the names of files containing "report"
for file in *; do
  if grep -q "report" <<< "$file"; then
    echo "$file"
  fi
done
```

These are just a few basic examples, but regular expressions can be used for a variety of tasks in Bash scripting, including string substitution, data validation, and more.

## Deep Dive

Regular expressions can get quite complex and there are many symbols and functions to learn. Some useful resources to help you dive deeper into regular expressions include:

- [Regular Expressions 101](https://regex101.com/): This online tool allows you to test and debug your regular expressions.
- [BashGuide - Regular Expressions](https://mywiki.wooledge.org/BashGuide/Patterns): This article provides a comprehensive guide to using regular expressions in Bash.
- [Mastering Regular Expressions](http://www.oreilly.com/pub/pr/2337): This book covers regular expressions in depth and is a great resource for learning all their intricacies.

Keep in mind that regular expressions can vary slightly between different programming languages and tools, so be sure to check the documentation for specifics.

## See Also

- [Bash scripting: How to use Regex (Regular Expressions) - Part 1](https://www.youtube.com/watch?v=lVyGWThzDjc)
- [Bash scripting: How to use Regex (Regular Expressions) - Part 2](https://www.youtube.com/watch?v=JDrG6rE6Zq0)
- [Bash scripting cheatsheet: Regex](https://devhints.io/bash#regex)