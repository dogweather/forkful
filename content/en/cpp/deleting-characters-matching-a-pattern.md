---
title:    "C++ recipe: Deleting characters matching a pattern"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

There are many situations where deleting characters matching a pattern can be useful in programming. Some examples include sanitizing user input, removing irrelevant or sensitive data from a string, or simply cleaning up messy data.

## How To

To delete characters matching a pattern in C++, we can use the `erase()` function from the `string` library. This function takes in two parameters - the starting index and the number of characters to be deleted. We can also use the `find()` function to locate the index of the first occurrence of the pattern in the string.

Here's an example code block showing how to delete all occurrences of the letter "a" from a string:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "apples and oranges are my favorite fruits";
    
    // Deleting all occurrences of "a"
    int index = 0;
    while ((index = str.find("a", index)) != string::npos) {
        str.erase(index, 1);
    }
    
    cout << str << endl;
    // Output: pples nd ornges re my fvorite fruits
}
```

## Deep Dive

The `erase()` function modifies the original string by deleting the specified characters. This can be a useful feature, but it also means that we need to be careful when using it. If we need to preserve the original string, we can make a copy and perform the deletion on the copy instead.

We can also use the `replace()` function to replace characters matching a pattern with a different character or string. This function takes in three parameters - the starting index, the number of characters to be replaced, and the replacement string.

There are also other ways to delete characters matching a pattern, such as using regular expressions or custom algorithms. It's important to carefully consider the specific needs of your program and choose the most efficient and effective method for your situation.

## See Also

- [C++ string library](https://www.cplusplus.com/reference/string/)
- [Using erase() and replace() in C++](https://www.geeksforgeeks.org/clear-erase-and-swap-in-cpp-stl/)
- [Regular expressions in C++](https://www.geeksforgeeks.org/regular-expression-regex-in-c/)