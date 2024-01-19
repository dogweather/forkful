---
title:                "Concatenating strings"
html_title:           "Python recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings involves combining two or more strings into one. Programmers use this to manage and manipulate texts efficiently, like constructing sentences.

## How to:

Here are a few ways to concatenate strings in Python:

1. Using the `+` Operator - It's the easiest way.
    ```Python
    str1 = 'Hello'
    str2 = 'World'
    concatenated_str = str1 + ' ' + str2
    print(concatenated_str)  # Outputs: Hello World
    ```

2. Using the `join()` Method - Ideal for joining a large number of strings.
    ```Python
    str_list = ['Hello', 'World']
    concatenated_str = ' '.join(str_list)
    print(concatenated_str)  # Outputs: Hello World
    ```

3. Using F-Strings – A programmer-friendly method available from Python 3.6 onward.
    ```Python
    str1 = 'Hello'
    str2 = 'World'
    concatenated_str = f'{str1} {str2}'
    print(concatenated_str)  # Outputs: Hello World
    ```

## Deep Dive

1. Historical Context: The `+` operator has been there since the earliest versions of Python. The `join()` method was introduced as a more efficient way to concatenate large numbers of strings, and from Python 3.6 onwards, f-strings became available to make formatting simpler.

2. Alternatives: You could use format strings (using `%`)
    ```Python
    var = "World"
    println("Hello %s" % var)  # Outputs: Hello World
    ```
3. Implementation Details: The `+` operator creates a new string and leaves the original strings unaltered. It's inefficient for large numbers of strings because of the overhead of creating new strings. The `join()` method is much faster because it precomputes the size of the final string and does the concatenation in one go. F-strings are just as efficient, but they also support embedded expressions and variable substitutions.

## See Also

1. [Python String Concatenation](https://realpython.com/python-string-concatenation/)
2. [Efficient String Concatenation in Python](https://waymoot.org/home/python_string/)
3. [F-Strings in Python](https://www.python.org/dev/peps/pep-0498/)