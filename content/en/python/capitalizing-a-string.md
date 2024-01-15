---
title:                "Capitalizing a string"
html_title:           "Python recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Capitalizing a string might seem like a simple task, but it is a common scenario in data analysis, content formatting, and user input validation. Whether you need to ensure consistency in your data or improve the appearance of your user interface, capitalizing a string can be a handy tool in your coding arsenal.

## How To
Capitalizing a string in Python is straightforward. You can use the built-in `capitalize()` method to capitalize the first letter of a string, or the `title()` method to capitalize the first letter of each word in a string.

```
```Python
name = "john doe"
print(name.capitalize())
# Output: John doe

title_case = "this is a sample string"
print(title_case.title())
# Output: This Is A Sample String
```

```

You can also use the `upper()` method to convert the entire string to uppercase or `lower()` method to convert it to lowercase.

```
```Python
text = "hello world"
print(text.upper())
# Output: HELLO WORLD

print(text.lower())
# Output: hello world```

```

## Deep Dive
The `title()` method and `capitalize()` method only capitalize the first letter of each word or the first letter of the string, respectively. However, they do not account for names or proper nouns that should always be capitalized. In situations where you want to ensure proper capitalization of names, you can use the `title()` method in combination with the `split()` method and a simple `for` loop.

```
```Python
name = "jane doe"
cap_name = []

for word in name.split():
    cap_name.append(word.capitalize())

print(' '.join(cap_name))
# Output: Jane Doe
```

Another useful tip is to use the `isalpha()` method to check if a string contains only letters. This can be helpful in scenarios where you want to capitalize only the first letter of words and not any numbers or special characters.

```
```Python
user_input = input("Enter a string: ")

if user_input.isalpha():
    print(user_input.title())
else:
    print("Invalid input, try again.")
```

## See Also
- [Python String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Stack Overflow: How to capitalize first letter of a string in Python](https://stackoverflow.com/questions/1549641/how-to-capitalize-the-first-letter-of-a-string-in-python)