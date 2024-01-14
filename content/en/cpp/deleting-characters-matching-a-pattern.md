---
title:    "C++ recipe: Deleting characters matching a pattern"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why

We often come across scenarios where we need to manipulate strings in our C++ code. One common task is deleting characters that match a certain pattern. This could be to remove unwanted characters from user input or to clean up data before processing it. In this blog post, we will explore how to delete characters matching a pattern in C++.

## How To

To delete characters matching a pattern in C++, we can use the `erase()` function from the `string` library. This function takes in two parameters - the starting index and the number of characters to be deleted. Let's take a look at an example:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "He1lo #World!";

    //Deleting the '#'
    str.erase(5, 1);

    //Deleting all the numbers
    for (int i = 0; i < str.length(); i++) {
        if (isdigit(str[i])) {
            str.erase(i, 1);
            i--; //As the string length decreases, so should the index
        }
    }

    cout << str << endl; //Expected output: Hello World!
    return 0;
}
```

In the above example, we use the `erase()` function to delete the `#` symbol and then loop through the string to delete all the numbers. Note that we have to decrement the index in our loop so that we don't skip over any characters.

## Deep Dive

The `erase()` function in C++ is actually an overloaded function. This means that it can take in different types of parameters depending on our needs. In the example above, we passed in the starting index and the number of characters to be deleted. However, we can also pass in an iterator to specify the starting and ending points for deletion.

Another important fact to note is that the `erase()` function returns a new string, which means that the original string is not modified. If we want to modify the original string, we can use the `replace()` function instead, which also has similar parameters as `erase()`.

## See Also

- [C++ Reference - string::erase](https://en.cppreference.com/w/cpp/string/basic_string/erase)
- [C++ Reference - string::replace](https://en.cppreference.com/w/cpp/string/basic_string/replace)
- [GeeksforGeeks - Delete a character from a string in C++](https://www.geeksforgeeks.org/how-to-remove-a-character-from-string-in-cpp/)

By utilizing the `erase()` function in C++, we can easily delete characters matching a pattern from our strings. It is a useful tool to have in our programming arsenal and can come in handy in many situations.