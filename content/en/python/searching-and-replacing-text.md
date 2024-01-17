---
title:                "Searching and replacing text"
html_title:           "Python recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text is a common task in programming where a specific string of text is targeted and replaced with another. It is done to make changes to a large amount of text quickly, saving time and reducing manual errors. Programmers often use this technique to efficiently modify code, update data, or make corrections in text files.

## How to:
```
# Using replace() function to replace a string
my_string = "Hello world!"
print(my_string.replace("Hello", "Hi"))

# Output: Hi world!

# Using regular expressions to replace multiple instances of a string
import re
my_string = "I love Python, do you?"
new_string = re.sub("Python", "coding", my_string)
print(new_string)

# Output: I love coding, do you?
```

## Deep Dive:
There have been several methods used for searching and replacing text throughout the history of programming. Traditional find and replace features were limited to specific applications or text editors, but now with the use of regular expressions, programmers have more flexibility in searching and replacing text. An alternative to using the replace() function is the translate() function which can be used to replace single characters. The implementation of searching and replacing text largely depends on the programming language used, but the basic concept remains the same.

## See Also:
- [Python documentation on replace() function](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Regular expressions in Python](https://www.programiz.com/python-programming/regex)
- [translate() function in Python](https://www.tutorialspoint.com/python/string_translate.htm)