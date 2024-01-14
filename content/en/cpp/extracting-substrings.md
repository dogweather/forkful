---
title:                "C++ recipe: Extracting substrings"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Why Extracting Substrings in C++ is Useful

When working with strings in C++, it's common to encounter situations where we need to extract a smaller portion of a larger string. This could be for tasks like pattern matching, data manipulation, or even just getting specific parts of a string for display. Whatever the reason may be, extracting substrings can be a useful and powerful tool in your programming arsenal.

## How To Extract Substrings in C++

To extract a substring in C++, we can use the `substr()` function defined in the `<string>` header. This function takes two parameters - the starting index of the substring and the length of the substring to be extracted. Let's take a look at an example code snippet to see how this works:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {

    string message = "Hello World!";
    
    //extracting "World"
    string sub = message.substr(6, 5);

    cout << sub << endl;
    
    return 0;
}
```

In the code above, we have a string variable `message` that stores the string "Hello World!". Using the `substr()` function, we extract a portion of this string starting at index 6 (which corresponds to the letter "W") and with a length of 5. This means that the substring "World" will be extracted and stored in the variable `sub`. We then print out the value of `sub` which should be "World".

We can also use variables or calculations for the parameters of `substr()`, giving us more flexibility in extracting substrings. Let's say we want to extract a substring that starts from the 6th letter from the end of the original string and has a length of 3. We can use the `size()` function to get the length of the string and perform some simple arithmetic to get the starting index and length of the substring:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {

    string message = "Hello World!";
    
    //extracting "old"
    int length = 3;
    int index = message.size() - 6;
    string sub = message.substr(index, length);

    cout << sub << endl;
    
    return 0;
}
```

The output of this code would be "old". As you can see, by using a combination of string functions and simple calculations, we can extract substrings in various ways.

## Deep Dive into Extracting Substrings in C++

The `substr()` function is just one way of extracting substrings in C++. There are other string functions like `find()` and `rfind()` that can also be used for this task. However, one thing to keep in mind is that indexing in C++ starts at 0, meaning that the first character in a string is at index 0, not 1.

Another important aspect to consider when extracting substrings is the potential for errors. For example, if the starting index we provide for `substr()` is greater than the string's length, it will result in an error. It's always a good practice to check the string's length and the starting index before extracting a substring to avoid these errors.

## See Also
- [C++ Strings](https://www.w3schools.com/cpp/cpp_strings.asp)
- [String Functions in C++](https://www.programiz.com/cpp-programming/library-function/string)
- [C++ Standard Library](https://www.geeksforgeeks.org/the-c-standard-template-library-stl/)