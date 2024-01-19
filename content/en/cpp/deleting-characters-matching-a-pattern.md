---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Deleting characters matching a pattern is about removing select elements from a string based on a set criterion. Programmers do this to manipulate data, often when cleaning up, standardizing, or transforming inputs.

## How to:
Let's roll up our sleeves and dive into the code. Below is an example of how you can achieve this in C++.
```C++
#include <algorithm>
#include <string>

std::string removeChars(std::string str, std::string charsToRemove) {
    for (char c: charsToRemove) {
        str.erase(std::remove(str.begin(), str.end(), c), str.end());
    }

    return str;
}

int main() {
    std::string str = "Remove vowels from this string.";
    std::string vowels = "aeiou";
    
    std::cout << "Before: " << str << "\n";
    std::cout << "After: " << removeChars(str, vowels) << "\n";

    return 0;
}
```
This chunk of code will output:
```
Before: Remove vowels from this string.
After: Rmv vwls frm ths strng.
```
Easy peasy.

## Deep Dive
The 'remove' algorithm is part of the Standard Template Library (STL) in C++. Old school programmers might remember when we had to manually loop over strings to do this before STL. 

We could also use regular expressions ('regex') for pattern matching. However, regex might have a steeper learning curve and can be overkill for simple scenarios.

Even simpler, we could use the 'remove_copy_if' or 'remove_if' algorithm with a lambda function to check our patterns. However, due to space limitations, we're not covering that here (Google is your friend here!).

## See Also
- [cplusplus.com](http://cplusplus.com/reference/algorithm/remove)
- [cppreference.com](http://en.cppreference.com/w/cpp/regex)
- [stackoverflow.com](http://stackoverflow.com/questions/5891610/how-to-remove-characters-from-a-string)
You're never alone on this journey, remember that! Plenty of fellow code-warriors out there ready to help.