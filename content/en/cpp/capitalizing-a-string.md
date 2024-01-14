---
title:                "C++ recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever received a string of text with the first letter of each word lowercase, and you needed to capitalize it? Or maybe you're just curious about how to manipulate strings in C++. Whatever the reason may be, capitalizing a string is a common task in programming and knowing how to do it can save you time and effort.

## How To

To capitalize a string in C++, you will need to use the "toupper()" function from the <cctype> library. This function takes in a single character as an argument and returns the uppercase version of that character. However, since we want to capitalize an entire string, we will need to loop through each character and apply the "toupper()" function. Let's take a look at an example: 

```C++
#include <iostream>
#include <cctype> // for toupper() function
#include <string> // for string data type

using namespace std;

int main() {
    // Initializing a string with lowercase letters 
    string str = "hello world";

    // Looping through each character in the string
    for (int i = 0; i < str.length(); i++) {
        // Using the toupper() function to capitalize each character
        str[i] = toupper(str[i]);
     }

     // Outputting the capitalized string
     cout << str << endl;

     return 0;
}
```

Output:
```C++
HELLO WORLD
```

As you can see, we used a for loop to go through each character in the string and apply the "toupper()" function to it. This resulted in our string being capitalized.

## Deep Dive

Now, let's take a deeper look at the code and understand why it works. When we declare our string, each character is stored as a numerical value (ASCII code). The lowercase letters have a numerical value that is different from their uppercase counterparts. For example, the lowercase letter "a" has a value of 97 while the uppercase "A" has a value of 65.

When we use the "toupper()" function, it takes in a character and returns its uppercase version by subtracting 32 from its ASCII code. This means, if the character is already uppercase, its ASCII code will not change. However, if it is lowercase, the function will subtract 32 from its ASCII code, resulting in the uppercase version.

In our example, we used a for loop to go through each character in the string and apply the "toupper()" function to it, effectively changing all lowercase letters to uppercase.

## See Also

- [C++ Strings](https://www.w3schools.com/cpp/cpp_strings.asp)
- [ASCII Table](https://www.ascii-code.com)
- [C++: toupper() function](https://www.programiz.com/cpp-programming/library-function/cctype/toupper)

By learning how to capitalize a string in C++, you have gained a useful skill for manipulating strings in your programs. Keep practicing and exploring the different ways to manipulate strings in C++ to become a more proficient programmer.