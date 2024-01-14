---
title:                "Python recipe: Extracting substrings"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substring extraction is a common task in string manipulation that allows us to isolate specific parts of a larger string. Whether you're working on data cleaning, text processing, or any other programming project involving strings, knowing how to extract substrings can save you time and effort.

## How To

To extract a substring, we will use the `slice` function in Python. This function takes in two arguments, the starting and ending indices of the substring we want to extract. Let's take a look at a simple example:

```Python
# create a string
my_string = "Hello, world!"

# extract the substring "world"
substring = my_string[7:12]

# print the output
print(substring)
```

The output of this code block will be `world`, as expected. It's important to note that the first index is inclusive, meaning it will be included in the substring, while the second index is exclusive, meaning it will not be included. In the example above, the 7th index is the letter "w" and the 12th index is the letter "d", so the substring will include the letters from index 7 up to, but not including, index 12.

We can also use negative indices to extract substrings. Negative indices count backwards from the end of the string, with -1 representing the last character. Let's see this in action:

```Python
# create a string
my_string = "Hello, world!"

# extract the substring "Hello"
substring = my_string[0:5]

# print the output
print(substring)
```

The output of this code block will be `Hello`, as expected. We can use the shortcut `[:5]` to indicate that we want to start at the beginning of the string and go up to, but not including, index 5. We can also use the shortcut `[5:]` to indicate that we want to start at index 5 and go all the way to the end of the string.

In addition, we can use the `slice` function with a third argument, the step size. This allows us to skip over characters in our substring. Let's take a look at an example:

```Python
# create a string
my_string = "Hello, world!"

# extract the substring "Hlool"
substring = my_string[::2]

# print the output
print(substring)
```

The output of this code block will be `Hlool`, as expected. The step size of 2 means we will only take every other character from the original string.

## Deep Dive

While the examples above show the basic usage of the `slice` function, there are many more ways to extract substrings in Python. For example, we can use a negative step size to reverse the string:

```Python
# create a string
my_string = "Hello, world!"

# extract the substring "dlrow"
substring = my_string[::-1]

# print the output
print(substring)
```

The output of this code block will be `dlrow`, as expected.

We can also use the `index()` or `find()` methods to find the index of a specific character in a string and use that index in our `slice` function to extract a substring. Another option is to use regular expressions to extract more complex substrings based on patterns.

## See Also

- [Python Documentation: String Methods](https://docs.python.org/3/library/string.html)
- [RealPython: The Ultimate Guide to Substrings in Python](https://realpython.com/python-substring/)
- [GeeksforGeeks: String indexing in Python](https://www.geeksforgeeks.org/python-string-indexing/)
- [W3Schools: Python String Methods](https://www.w3schools.com/python/python_ref_string.asp)