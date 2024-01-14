---
title:                "C++ recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever wondered how a computer is able to determine the length of a string? You might have encountered this problem while working on a programming project or while debugging a code. Understanding how to find the length of a string is a basic but essential skill in programming, especially in languages like C++.

## How To

Finding the length of a string in C++ is a straightforward process. The standard library function used for this purpose is the `strlen()` function. It takes a null-terminated string as an argument and returns the length of the string. Let's take a look at an example:

```C++
#include <iostream>
#include <cstring>

using namespace std;

int main() {
    char str[] = "Hello World!";
    int len = strlen(str);

    cout << "The length of the string is: " << len << endl;
    return 0;
}
```

Output: The length of the string is: 12

In this code, we first declare a character array `str` and initialize it with the string "Hello World!". Then, we use the `strlen()` function to determine the length of the string and store it in the variable `len`. Finally, we print out the result using the `cout` statement.

Another way to find the length of a string is by using a loop and counting the number of characters in the string until the null character `\0` is encountered. This method is useful when dealing with strings that have varying lengths or when working with custom functions. Here is an example:

```C++
#include <iostream>

using namespace std;

int main() {
    char str[] = "Programming";
    int len = 0;

    while (str[len] != '\0') {
        len++;
    }

    cout << "The length of the string is: " << len << endl;
    return 0;
}
```

Output: The length of the string is: 11

In this code, we declare a character array `str` and initialize it with the string "Programming". Then, we use a `while` loop to loop through the string and count the number of characters until the null character is encountered. The variable `len` is incremented by 1 for each character, giving us the length of the string.

## Deep Dive

Now that we know how to find the length of a string, let's take a deeper look at the `strlen()` function. This function is defined in the header file `<cstring>` and is used to determine the length of a null-terminated string. It works by counting the number of characters in the string until the null character is encountered. The complexity of this function is O(n) where n is the number of characters in the string.

It is important to note that the `strlen()` function does not count the null character as part of the string's length. For example, in the code `char str[] = "Hello";`, the length of the string returned by `strlen()` would be 5, not 6. This is because the null character is added automatically at the end of the string by the compiler.

## See Also

- [C++ Strings](https://www.geeksforgeeks.org/c-string-class-and-its-applications/)
- [C++ Standard Library](https://www.learncpp.com/cpp-tutorial/the-standard-c-library/)
- [Character Arrays in C++](https://www.tutorialspoint.com/cplusplus/cpp_arrays.htm)