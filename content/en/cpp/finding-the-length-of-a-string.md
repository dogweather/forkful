---
title:    "C++ recipe: Finding the length of a string"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why

String manipulation is a common task in software development, and finding the length of a string is a crucial part of it. Whether you are creating a word processing program or building a website, knowing how to find the length of a string can save you time and effort.

## How To

To find the length of a string in C++, we can use the `length()` function from the `string` class. Let's take a look at an example:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "Hello World";
    int length = str.length();

    cout << "The length of the string is: " << length << endl;

    return 0;
}
```

Output:
```
The length of the string is: 11
```

In this example, we first declare a string variable `str` and assign it the value "Hello World". Then, we use the `length()` function to find the length of the string and store it in an integer variable `length`. Finally, we print out the length of the string using `cout`.

We can also use the `size()` function to achieve the same result. Here's an example:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "Hello World";
    int size = str.size();

    cout << "The size of the string is: " << size << endl;

    return 0;
}
```

Output:
```
The size of the string is: 11
```

The `length()` and `size()` functions have the same functionality, and you can use either of them to find the length of a string.

## Deep Dive

Behind the scenes, the `length()` and `size()` functions use a hidden variable stored inside the `string` class called `length_` to keep track of the string's length. This variable is initialized when the string is created and is updated whenever the string is modified. By accessing this variable, the `length()` and `size()` functions can quickly return the length of the string without having to iterate through the entire string.

It is essential to note that the length of a string in C++ does not include the null character `'\0'` at the end of the string. This is because the null character is only used to mark the end of the string and is not considered as part of the string's contents.

## See Also

- [C++ string class documentation](https://www.cplusplus.com/reference/string/string/)
- [C++ string length and size functions documentation](https://www.cplusplus.com/reference/string/string/length/)
- [C++ string length and size functions tutorial](https://www.learncpp.com/cpp-tutorial/3-8a-stdstring-length-and-size/)