---
title:    "C++ recipe: Converting a string to lower case"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case can be a useful task in many programming applications. It allows for more flexibility and consistency in string comparisons, making it easier to handle user input or manipulate text data.

## How To

To convert a string to lower case in C++, we will use the `tolower()` function from the `<cctype>` library. This function takes in a character and converts it to its corresponding lower case letter, or leaves it unchanged if it is not a letter.

```C++
#include <iostream>
#include <cctype>

using namespace std;

int main() {
    string str = "Hello World!";
    for(int i=0; i<str.length(); i++) {
        str[i] = tolower(str[i]); // converting each character to lower case
    }
    cout << str << endl; // output: hello world!
    
    return 0;
}
```

We can also use this function in combination with the `transform()` function from the `<algorithm>` library, which allows us to apply a function to each element in a range. This can be useful for converting an entire string to lower case without using a loop.

```C++
#include <iostream>
#include <algorithm>
#include <cctype>

using namespace std;

int main() {
    string str = "Hello World!";
    transform(str.begin(), str.end(), str.begin(), ::tolower); // converting entire string to lower case
    cout << str << endl; // output: hello world!
    
    return 0;
}
```

## Deep Dive

In C++, strings are stored as a sequence of characters in memory. This means that each character has a corresponding ASCII code, which is used for comparison and manipulation. The ASCII code for lower case letters starts at 97, while the code for upper case letters starts at 65. By subtracting 32 from the ASCII code of an upper case letter, we get the corresponding lower case letter.

The `tolower()` function uses this concept by taking in a character and returning its ASCII code if it is already a lower case letter, or the ASCII code of its corresponding lower case letter if it is an upper case letter. The returned ASCII code is then converted back to a character and replaces the original character in the string.

## See Also

- [C++ String Manipulation: Converting to Upper or Lower Case](https://www.programiz.com/cpp-programming/library-function/cctype/tolower)
- [C++ <cctype> Header](https://www.cplusplus.com/reference/cctype/)