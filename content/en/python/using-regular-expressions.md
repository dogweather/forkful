---
title:    "Python recipe: Using regular expressions"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why
Regular expressions are a powerful tool in the world of programming. They allow you to easily manipulate and search through text using a concise and flexible syntax. Whether you are a beginner or an experienced programmer, regular expressions can greatly improve your efficiency and make your code more robust.

## How To
Using regular expressions in Python is relatively straightforward. First, you must import the `re` module. Then, you can use the `re.search()` function to search for a particular pattern in a string. Let's walk through an example:

```
# Import the re module
import re

# Define a string to search through
text = "This is a sample sentence that contains the word Hello."

# Use re.search() to find the position of the word "Hello"
match = re.search("Hello", text)

# Check if the word was found
if match:
    print("The word Hello was found at position:", match.start())
else:
    print("The word Hello was not found.")
```

In this example, the `re.search()` function returns an object that contains information about the match, including the starting position of the match. If the word was found, the starting position is printed. If not, a message stating that the word was not found is printed.

Regular expressions also offer a variety of control characters and metacharacters to further customize your search pattern. For example, the `^` character matches the beginning of a string and the `$` character matches the end. This allows you to search for words only at the beginning or end of a string, rather than anywhere it appears. You can also use quantifiers such as `*` and `+` to specify how many times a certain pattern should appear.

For a more in-depth guide on using regular expressions in Python, check out the [official documentation](https://docs.python.org/3/library/re.html) for the `re` module.

## Deep Dive
Regular expressions may seem daunting at first, but once you understand the basics, they can greatly enhance your ability to manipulate and analyze text data. One key concept to understand is the concept of "greedy" and "non-greedy" matching.

By default, regular expressions use "greedy" matching, meaning they will try to match the longest possible string that satisfies the search pattern. For example, given the string `Hello World`, the pattern `H.*o` would match the entire string, while the pattern `H.*o?` would only match the letters `Hello`.

On the other hand, you can use "non-greedy" matching by adding a `?` after a quantifier. This will make the pattern match the shortest possible string that satisfies the search pattern. Using our previous example, the pattern `H.*?o` would only match the letters `He`.

For more tips and tricks on using regular expressions, check out this [comprehensive tutorial](https://www.regular-expressions.info/tutorial.html).

## See Also
- [Official Python `re` documentation](https://docs.python.org/3/library/re.html)
- [Regular expressions tutorial](https://www.regular-expressions.info/tutorial.html)
- [Online regular expression tester](https://regexr.com/)