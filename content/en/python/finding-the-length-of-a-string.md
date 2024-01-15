---
title:                "Finding the length of a string"
html_title:           "Python recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why 

Finding the length of a string is a common task in programming. It allows you to accurately determine the size of a string, which can be useful in various string manipulation operations. Plus, it's a simple and practical skill to have in your programming toolbox.

## How To

To find the length of a string in Python, you can use the built-in `len()` function. Here's an example of how it works:

```Python
my_string = "Hello world!"
print(len(my_string))
```

This code will output `12`, as there are 12 characters in the string "Hello world!". 

You can also use this function on different types of data, such as lists and tuples:

```Python
my_list = ["apple", "orange", "banana"]
print(len(my_list))
```

This will output `3`, as there are 3 items in the list.

## Deep Dive

While the `len()` function may seem simple, it is actually quite powerful. It can handle different types of data and can even be customized for your own objects and classes. Additionally, this function is optimized for efficiency, so it's a reliable and fast way to find the length of a string.

One thing to note is that the length returned by `len()` may not always be the same as the number of visible characters in a string. This is because some characters, such as emojis or special symbols, may count as multiple characters in the string. This is something to keep in mind when working with non-English or non-standard characters.

## See Also

- Python documentation on `len()` function: https://docs.python.org/3/library/functions.html#len
- GeeksforGeeks article on finding the length of a string in Python: https://www.geeksforgeeks.org/python-string-length-len/
- Real Python tutorial on string manipulation: https://realpython.com/python-strings/