---
title:    "C++ recipe: Finding the length of a string"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

As a programmer, it's important to have a good understanding of the basics of any programming language. One such basic function in C++ is finding the length of a string. Whether you're new to C++ or brushing up on your skills, knowing how to find the length of a string is essential for writing efficient and error-free code.

## How To

To find the length of a string in C++, you can use the built-in function `strlen()`, which is included in the `<string.h>` library.

```C++
#include <iostream> 
#include <string.h> 

using namespace std; 

int main() 
{ 
  char str[] = "Hello World!"; 
  int length = strlen(str); 
  cout << "The length of the string is: " << length << endl; 
  return 0; 
} 
```

This code will output: `The length of the string is: 12`. 

You can also use the `size()` function to find the length of the string, which is included in the `<string>` library.

```C++
#include <iostream> 
#include <string> 

using namespace std; 

int main() 
{ 
  string str = "Hello World!"; 
  int length = str.size(); 
  cout << "The length of the string is: " << length << endl; 
  return 0; 
} 
```

This code will also output: `The length of the string is: 12`.

## Deep Dive

The `strlen()` function calculates the length of a string by counting the number of characters until it reaches the null-terminating character, `'\0'`. This character marks the end of a string in C++.

Similarly, the `size()` function counts the number of characters in a string until it reaches the null-terminating character. However, in this case, the null-terminating character is included in the count.

It's important to note that `strlen()` and `size()` only work on null-terminated strings, meaning the string must end with `'\0'`. If the string does not have this character, the functions may return an incorrect length value.

## See Also

- [C++ string functions](https://www.programiz.com/cpp-programming/string-functions)
- [Difference between `strlen()` and `size()`](https://stackoverflow.com/questions/825187/difference-between-strlen-and-size-in-c)