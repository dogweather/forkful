---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# What & Why?
Finding the length of a string means determining how many characters it has. It’s a common task because many calculations, validations, and operations hinge upon this info. 

# How to:

Here is the most common way to find a string’s length using the `std::string` type’s `size()` or `length()` methods:

```C++
#include <iostream>
#include <string>

int main() {
    std::string s = "Hello, World!";

    std::cout << "length: " << s.size() << "\n";
    std::cout << "length: " << s.length() << "\n";

    return 0;
}
```

You'll see:

```
length: 13
length: 13
```

# Deep Dive

Historically, C-style strings (null-terminated character arrays) were used before the `std::string` type introduction in C++. They calculated lengths by manually iterating over the array until hitting a null character:

```C++
char s[] = "Hello, World!";
int i = 0;
while (s[i] != '\0') ++i;
```

It's less safe and less simple than `std::string`'s methods due to risk of not finding the null character, which leads to undefined behavior.

In C++, `std::string`'s `length()` and `size()` methods are interchangeable. They're O(1) operations because `std::string` stores the string's size as an internal variable.

Strangely enough, both these methods were kept in the standard for compatibility with other STL containers’ API, not because they do different things.

Precisely, the existence of two methods doing the same thing might seem confusing, but it's merely a way to fit in different contexts: `size()` in container-generic programming, and `length()`, that feels semantically more correct when working with text.

# See Also

1. [C++ Reference | std::basic_string::length](https://en.cppreference.com/w/cpp/string/basic_string/length)
2. [C++ Reference | std::basic_string::size](https://en.cppreference.com/w/cpp/string/basic_string/size)
3. [Learn the difference between size() and length()](https://stackoverflow.com/questions/905222/why-does-stdstring-have-size-and-length-members-function-that-are-the-sam)
4. [More about C-Style Strings](https://www.learncpp.com/cpp-tutorial/c-style-strings/)