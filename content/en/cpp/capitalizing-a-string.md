---
title:    "C++ recipe: Capitalizing a string"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing strings may seem like a small and insignificant task in programming, but it can have a big impact on the overall functionality of a program. By capitalizing a string, we can ensure that data is stored and displayed in a consistent and visually appealing manner. In this blog post, we will take a closer look at why capitalizing strings is important and how to do it using C++.

## How To 

Capitalizing a string in C++ is a simple task that can be achieved through a few lines of code. Let's take a look at an example:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Define a string
    string str = "hello world";

    // Capitalize the first letter
    str[0] = toupper(str[0]);

    // Print the capitalized string
    cout << str << endl;

    // Output: Hello world
    return 0;
} 
```

In the above code, we used the `toupper()` function to capitalize the first letter of the string. This function is a part of the `<cctype>` header and takes in a character as a parameter, which in this case is the first character of our string.

If we want to capitalize the entire string, we can use a `for` loop to iterate through each character and capitalize it.

```C++
// Capitalize the entire string
for (int i = 0; i < str.length(); i++) {
    str[i] = toupper(str[i]);
}
```

Let's see the output of this code:

```C++
// Output: HELLO WORLD
```

As you can see, our string is now completely capitalized.

## Deep Dive

In C++, strings are stored as an array of characters. This means that we can access and manipulate individual characters in the string using indexing. The `toupper()` function is a standard library function that is used to convert a lowercase character to uppercase.

It is important to note that the `toupper()` function only converts lowercase letters to uppercase. If we try to use it on a character that is already uppercase, it will remain unchanged.

In addition, the `toupper()` function only works for ASCII characters. This means that if you have special characters or non-English characters in your string, they will not be affected by the function.

## See Also

For more information on strings and C++ programming, check out these resources:

- [LearnCpp.com](https://www.learncpp.com/) - A comprehensive tutorial on C++ programming
- [Cplusplus.com](http://www.cplusplus.com/reference/cctype/toupper/) - Detailed documentation on the `toupper()` function
- [GeeksforGeeks.org](https://www.geeksforgeeks.org/transform-a-string-in-c/) - Different methods for manipulating strings in C++