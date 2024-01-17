---
title:                "문자열 대문자 변환"
html_title:           "Python: 문자열 대문자 변환"
simple_title:         "문자열 대문자 변환"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# What & Why?

Capitalizing a string means converting the first character of each word in the string to uppercase and the remaining characters to lowercase. Programmers often do this to make strings more visually appealing, follow proper grammar rules, or enhance search functionality.

# How to:

To capitalize a string in Python, you can use the built-in `title()` function. Here is an example:

```Python
my_string = "hello world"
print(my_string.title()) # Output: Hello World
```

You can also use the `capitalize()` method to capitalize only the first letter of the string. Here is an example:

```Python
my_string = "hello world"
print(my_string.capitalize()) # Output: Hello world
```

# Deep Dive:

Capitalized strings have been used since the early days of computing as a way to format text for display or printing. In Python, there are also alternatives for capitalizing strings such as using regular expressions or creating custom functions. However, the `title()` function is the most widely used method due to its simplicity and efficiency.

The `title()` function follows the Titlecase convention, which is a writing style that capitalizes the first letter of each word (except for articles, conjunctions, and prepositions) within a sentence. This makes the string more visually appealing and easier to read.

# See Also:

- [Python String Methods](https://www.w3schools.com/python/python_ref_string.asp)
- [Titlecase Convention](https://en.wikipedia.org/wiki/Letter_case#Title_case)