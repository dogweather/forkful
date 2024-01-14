---
title:                "C++ recipe: Capitalizing a string"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Why

Have you ever come across a situation where you needed to capitalize a string in your C++ code? Perhaps you want to display the user's name in all capital letters or format a title to follow proper capitalization rules. Whatever the case may be, knowing how to capitalize a string can be a useful skill to have in your programming arsenal.

##How To

To capitalize a string in C++, you can use the `std::toupper()` function from the `<cctype>` library. This function takes in a character as an argument and returns the corresponding uppercase letter. Here is an example of how you can use it:

```C++
#include <iostream>
#include <cctype>

int main() {
    std::string name = "john";
    for (char& c : name) {
        c = std::toupper(c);
    }
    std::cout << name << std::endl;
    return 0;
}
```

The output of this code would be `JOHN`, as each character in the string `name` has been converted to uppercase using the `std::toupper()` function.

You can also use the `std::string::toupper()` function to capitalize a string. This is a member function of the `std::string` class and works similarly to the `std::toupper()` function.

```C++
#include <iostream>
#include <string>

int main() {
    std::string title = "the great gatsby";
    title[0] = toupper(title[0]); // capitalize first letter
    std::cout << title << std::endl;
    return 0;
}
```

The output of this code would be `The great gatsby`, as only the first letter of the string `title` has been converted to uppercase.

##Deep Dive

There are a few things to keep in mind when capitalizing a string in C++. Firstly, both the `std::toupper()` and `std::string::toupper()` functions only work with ASCII characters. This means that characters from other languages or symbols may not be converted to uppercase correctly.

Additionally, the `std::toupper()` function takes in an `int` as an argument, not a `char`. This is because it also supports wide characters (represented by the `wchar_t` type) which may have different values from regular characters.

Lastly, it is important to note that the `std::toupper()` function only converts the character to its uppercase equivalent if one exists. If the character is already uppercase, it remains unchanged. This can be an issue if you want to strictly enforce uppercase letters in your string.

##See Also

- [cctype - cppreference.com](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [std::string - cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string/toupper)