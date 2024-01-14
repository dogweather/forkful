---
title:    "Python recipe: Deleting characters matching a pattern"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

In certain situations, you may find yourself needing to delete characters from a string that match a specific pattern. This could be for data cleaning purposes or for processing user input. Knowing how to delete these characters in a simple and efficient way can save you time and frustration in your coding journey.

## How To

To delete characters matching a pattern in Python, we can use the `re` library. This library allows us to perform regular expression operations on strings. Here's an example of deleting all vowels from a string using regular expressions:

```
import re

# Define the string
text = "Hello, world!"

# Use the re.sub() function to replace all vowels with an empty string
clean_text = re.sub(r'[aeiouAEIOU]', '', text)

# Print the clean string
print(clean_text)
```

The output of this code will be `Hll, wrld!`, as all vowels have been deleted from the original string. Let's break down this code line by line:

- First, we import the `re` library to use its functions.
- Then, we define the original string that we want to modify.
- Next, we use the `re.sub()` function, which takes in three parameters: the pattern we want to replace, what we want to replace it with, and the string on which we want to perform the operation.
- In this case, our pattern is `r'[aeiouAEIOU]'`, which is a regular expression that matches all vowels, both lowercase and uppercase. The second parameter is an empty string, meaning we want to replace all vowels with nothing. And the third parameter is our original string.
- The result of the `re.sub()` function is assigned to a new variable called `clean_text`.
- Finally, we print the clean string, and we can see that all vowels have been successfully deleted.

## Deep Dive

Regular expressions, also known as regex, are a powerful tool for string manipulation. They allow us to define patterns and search for those patterns within strings. In our example, we used the `[aeiouAEIOU]` pattern, which is called a character class. This pattern tells the `re` library to search for any of the characters within the brackets and replace them with the given string.

However, regex can be much more complex than that. We can use special characters such as `+` (one or more occurrences), `*` (zero or more occurrences), and `?` (zero or one occurrence) to define our patterns. We can also use parentheses for grouping and backslashes to escape special characters.

It may seem overwhelming at first, but mastering regular expressions can greatly improve your data cleaning and text processing skills. You can learn more about regex in the [Python documentation](https://docs.python.org/3/howto/regex.html) and practice using it with [online tools](https://regex101.com/) or [coding challenges](https://www.hackerrank.com/domains/regex).

## See Also

Here are some additional resources that can help you further understand and practice deleting characters matching a pattern in Python:

- [Python Regex Tutorial: Matching Character Sets](https://realpython.com/regex-python/#matching-character-sets) by Real Python
- [Python Regular Expression Cheatsheet](https://www.debuggex.com/cheatsheet/regex/python) by Debuggex
- [RegexOne](https://regexone.com/) - a interactive tutorial for learning and practicing regular expressions with Python and other languages.