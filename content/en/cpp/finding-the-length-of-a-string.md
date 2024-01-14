---
title:                "C++ recipe: Finding the length of a string"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
As a beginner in C++ programming, one of the most fundamental concepts that you should understand is finding the length of a string. This is a crucial skill that is used in many different applications, from simple string manipulations to more complex algorithms. In this blog post, we will explore why it is important to know how to find the length of a string and how it can be done efficiently.

## How To
To find the length of a string in C++, we can use the built-in function `strlen()`. This function takes in a string as its parameter and returns the length of that string. Here is an example of how we can use this function in our code:

```C++
#include <iostream>
#include <cstring>

using namespace std;

int main() {
    char str[] = "Hello World!";
    int length = strlen(str);
    cout << "The length of the string is: " << length << endl;
    
    return 0;
}
```

**Output:**
```
The length of the string is: 12
```

We can also use a for loop to manually count the number of characters in a string until we encounter the null character, which marks the end of the string. Here is an example of how we can do this:

```C++
#include <iostream>

using namespace std;

int main() {
    char str[] = "Hello World!";
    int length = 0;
    
    for (int i = 0; str[i] != '\0'; i++) {
        length++;
    }
    
    cout << "The length of the string is: " << length << endl;
    
    return 0;
}
```

**Output:**
```
The length of the string is: 12
```

## Deep Dive
When we use the `strlen()` function, it internally uses a pointer that starts from the beginning of the string and iterates through it until it reaches the null character. This makes it a faster and more efficient way of finding the length of a string compared to using a for loop. However, it is important to note that the `strlen()` function returns the length of the string up until the null character, so if our string contains special characters or symbols, it may not give an accurate length.

It is also worth mentioning that `strlen()` belongs to the `cstring` library, so we need to include that in our code in order to use this function.

## See Also
- [String.h reference](https://www.cplusplus.com/reference/cstring/)
- [C++ documentation](https://en.cppreference.com/w/cpp)
- [Learning C++: A Step-by-Step Guide](https://www.educative.io/blog/learning-cpp-a-step-by-step-guide)