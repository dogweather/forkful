---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String interpolation is a process substituting placeholders with values within a string. It makes manipulating and formatting strings easier, enhancing readability and simplifying the debugging process.

## How to:

To use string interpolation in Python, place variables inside `{}` in your string. Use an `f` before the string to tell Python to interpolate it.

```Python
name = "John"
print(f"Hello, {name}!")  
```

Output:

```Python
Hello, John!
```

You can also do operations within the `{}`:

```Python
a = 5
b = 3
print(f"{a} plus {b} equals {a+b}")
```

Output:
    
```Python
5 plus 3 equals 8
```

## Deep Dive

Historically, Python had `%` formatting and the `str.format()` function before f-strings (formatted string literals) were introduced in Python 3.6. All of the methods, f-strings are not only more readable but also faster. 

```Python
name = "John"
# old % formatting
print("Hello, %s!" % name)  

# .format() function
print("Hello, {}!".format(name))  
```

However, these methods are still available for use but are less efficient than f-strings.

You can even include expressions inside the `{}` in an f-string. Python evaluates these expressions at runtime and includes the results in the returned string.

## See Also

1. Python documentation on [Formatted string literals](https://docs.python.org/3/reference/lexical_analysis.html#f-strings)
2. Python documentation on [String Formatting Operations](https://docs.python.org/2/library/stdtypes.html#string-formatting)
3. A useful blog post on [Python String Interpolation](https://www.digitalocean.com/community/tutorials/an-introduction-to-string-functions-in-python-3)
4. Python documentation on [.format()](https://docs.python.org/3/library/stdtypes.html#str.format)