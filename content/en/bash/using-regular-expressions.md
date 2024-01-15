---
title:                "Using regular expressions"
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why
Regular expressions (regex for short) are powerful tools used for pattern matching and text manipulation. Learning how to use regex in Bash can greatly enhance your scripting skills and make your code more efficient and concise.

## How To
First, we need to understand the basic syntax of regular expressions in Bash. To start, we use the `=~` operator to match a string against a regex pattern. For example, let's say we want to check if a string contains the word "hello", we can use the following code:
```Bash
if [[ "hello world" =~ hello ]]; then
  echo "Match found!"
fi
```

In the above code, we are using the `=~` operator to check if the string "hello world" contains the pattern "hello". If it does, the code inside the `if` statement will be executed and we will see the output "Match found!".

We can also use regular expressions for more complex pattern matching. Here are some common regex metacharacters and their meanings:

- `.` - Matches any single character.
- `*` - Matches zero or more occurrences of the previous character.
- `+` - Matches one or more occurrences of the previous character.
- `?` - Matches zero or one occurrence of the previous character.
- `^` - Matches the beginning of a string.
- `$` - Matches the end of a string.
- `[]` - Matches any character within the brackets.
- `()` - Groups characters together as a single element.
- `\` - Escapes metacharacters, allowing them to be treated literally.

Let's see an example of using regex with these metacharacters:
```Bash
if [[ "helloooo" =~ ^hel+o*$ ]]; then
  echo "Match found!"
fi
```

In this code, we are using the `^` and `*` metacharacters to match strings starting with "hel" and ending with any number of "o"s. If we run this code, we will see the output "Match found!".

## Deep Dive
It is important to note that Bash uses a different type of regular expression compared to other programming languages. Bash uses POSIX extended regular expressions, while other languages may use Perl-Compatible Regular Expressions (PCRE). This means that some advanced features, such as lookaheads and lookbehinds, are not supported in Bash regex.

Additionally, Bash regex is also case-sensitive by default. To make it case-insensitive, we can use the `shopt` command:
```Bash
shopt -s nocasematch
if [[ "hello" =~ HELLO ]]; then
  echo "Match found!"
fi
```
In this code, we are turning on the `nocasematch` option to make our regex case-insensitive. Now, if we run this code, we will see the output "Match found!" even though "HELLO" is not in the same case as our string "hello".

## See Also
- [Bash Guide for Beginners](http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_04_01.html)
- [GNU Bash Manual - Pattern Matching](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Pattern-Matching)
- [Regular Expressions Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)

Regular expressions may seem daunting at first, but with practice, they can greatly improve your scripting skills. So go ahead and dive deeper into the world of regex, and you'll see the benefits it can bring to your Bash scripts. Happy coding!