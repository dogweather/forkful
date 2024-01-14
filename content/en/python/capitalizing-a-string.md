---
title:    "Python recipe: Capitalizing a string"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why 
Capitalizing a string is a common task in programming, especially when dealing with user input or displaying data. By capitalizing a string, we ensure that the first letter of each word is in uppercase, making it easier to read and understand.

## How To
To capitalize a string in Python, we can use the `capitalize()` method. This method capitalizes the first letter of a string, while leaving the rest of the letters as-is. Let's see how this works with a simple example:

```Python
# creating a string variable
my_string = "hello world"

# capitalizing the string
capitalized_string = my_string.capitalize()

# printing the result
print(capitalized_string) # output: Hello world 
```

We can also use the `title()` method to capitalize the first letter of each word in a string, not just the first letter of the entire string. Let's see how this looks:

```Python
# creating a string variable
my_string = "hello world"

# capitalizing the string
capitalized_string = my_string.title()

# printing the result
print(capitalized_string) # output: Hello World
```

Both `capitalize()` and `title()` methods return a new string, leaving the original string unchanged. Alternatively, we can use the `upper()` method to capitalize all letters in a string:

```Python
# creating a string variable
my_string = "hello world"

# capitalizing the string
capitalized_string = my_string.upper()

# printing the result
print(capitalized_string) # output: HELLO WORLD 
```

## Deep Dive
It's important to note that the `capitalize()` method only capitalizes the first letter of a string if it is a lowercase letter. If the first letter is already in uppercase or is not a letter at all (e.g. a number or symbol), it will remain unchanged. For example:

```Python
# creating a string variable
my_string = "123hello world"

# capitalizing the string
capitalized_string = my_string.capitalize()

# printing the result
print(capitalized_string) # output: 123hello world
```

To overcome this limitation, we can use the `title()` method, which capitalizes the first letter of each word, regardless of its current case. However, this method may not be suitable for all cases, such as when dealing with acronyms or abbreviations.

## See Also
For more information on string manipulation in Python, check out the following resources:

- Official Python documentation on string methods: https://docs.python.org/3/library/stdtypes.html#str.capitalize
- Real Python blog on string manipulation: https://realpython.com/python-strings/
- GeeksforGeeks article on string methods in Python: https://www.geeksforgeeks.org/python-string-methods-set-1-isdigit-isnumeric-decode-encode/

Happy coding!