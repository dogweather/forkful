---
title:                "Using regular expressions"
html_title:           "C++ recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, commonly referred to as "regex", are powerful tools for searching, filtering, and manipulating text data. They allow for complex pattern matching, making tasks like data validation, text parsing, and search and replace operations much more efficient. Engaging in the use of regular expressions can greatly improve productivity and streamline programming tasks.

## How To

To use regular expressions in C++, we need to include the `<regex>` library. Here's a simple example of a regular expression that finds all words starting with the letter "t":

```C++
#include <iostream>
#include <regex>

using namespace std;

int main() {
  regex pattern("t[a-z]*");

  string text = "The cat sat on the mat.";

  sregex_iterator currentMatch(text.begin(), text.end(), pattern);
  sregex_iterator lastMatch;

  while(currentMatch != lastMatch) {
    smatch match = *currentMatch;
    cout << match.str() << endl;
    currentMatch++;
  }

  return 0;
}
```

**Output:**

```
the
the
t
on
the
t
```

In this example, we declare a regex object with the pattern we want to match. Then, we initialize two iterators to loop through the string and find all matches for our pattern. For each match, we use the `str()` method to extract the matching text and print it. We then increment the current match iterator until it reaches the last match.

Regular expressions also allow for advanced features like character classes, quantifiers, and grouping. For more in-depth examples and explanations, check out the links in the "See Also" section.

## Deep Dive

Regular expressions follow a specific syntax and have a variety of special characters and rules. Some key concepts to understand are:

- **Metacharacters:** Special characters that have a specific meaning in regular expressions, such as `*` for zero or more repetitions, `.` for any single character, and `[]` for character classes.
- **Quantifiers:** These specify how many times a pattern or character should be repeated, such as `+` for one or more repetitions, `?` for zero or one repetitions, and `{x}` for a specific number of repetitions.
- **Character classes:** These represent a set of characters and allow for more flexibility in matching. For example, `[A-Z]` represents any uppercase letter.
- **Grouping:** Using parentheses `()` allows for grouping of characters in a pattern, which can be useful for extracting specific parts of a text.

Understanding these concepts can help make regular expressions easier to work with and allow for more precise matching.

## See Also

- [C++ Regex Tutorial](https://www.cplusplus.com/reference/regex/tutorial/)
- [Regular Expressions Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [Advanced Regular Expressions in C++](https://www3.ntu.edu.sg/home/ehchua/programming/howto/Regexe.html)