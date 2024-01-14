---
title:    "C++ recipe: Extracting substrings"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why
Substring extraction is a common task in programming, especially when dealing with strings. It allows us to extract a specific part of a string that we are interested in and use it for further processing. Whether you are a beginner or an experienced programmer, understanding how to extract substrings can be a useful skill to have in your arsenal.

## How To
To extract a substring, we first need a string to work with. For this example, let's use the string "Hello world!". We will also need to specify the starting index and the length of the substring we want to extract. In this case, we want to extract the word "world", which starts at index 6 and has a length of 5 characters.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "Hello world!";

    // Extract substring starting at index 6 with a length of 5
    string substr = str.substr(6, 5);

    // Output: world
    cout << substr;

    return 0;
}
```

In the above code, we use the `substr()` function which is a member of the `string` class in C++. This function takes in two arguments: the starting index and the length of the substring. It then returns a new string containing the extracted substring.

You can also extract substrings in a more dynamic way using variables for the starting index and the length. For example:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "Hello world!";
    int start = 6;
    int length = 5;

    // Extract substring using variables
    string substr = str.substr(start, length);

    // Output: world
    cout << substr;

    return 0;
}
```

## Deep Dive
Under the hood, the `substr()` function uses a `std::basic_string` as its data structure. It copies the specified substring from the original string and creates a new string object. This means that modifying the extracted substring will not affect the original string.

It is worth noting that the starting index for substrings in C++ is 0-based, meaning that the first character of a string has an index of 0. If you specify a negative starting index, it will start from the end of the string.

You can also use `substr()` to extract a substring until the end of a string by only specifying the starting index. For example, if we want to extract "world!" from our original string, we can do so by using `substr(6)`, as shown below:

```C++
string str = "Hello world!";

// Extract substring from index 6 until the end
string substr = str.substr(6); // Output: world!
```

## See Also
- [C++ Strings](https://www.geeksforgeeks.org/c-strings/)
- [String.substr() in C++](https://www.geeksforgeeks.org/string-substr-in-cpp/)
- [C++ Standard Library - Substrings](https://en.cppreference.com/w/cpp/string/basic_string/substr)