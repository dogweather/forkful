---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

# String Interpolation in C++

## What & Why?
String interpolation is a process of injecting values into a string. It makes code cleaner, easier to understand, and reduces chances of errors.

## How to:
Let's get straight into code:

```C++
#include<iostream>
#include<string>

int main() {
   std::string name = "James";
   int age = 22;

   std::cout << "Hello, my name is " << name << " and I am " << age << " years old.";
   return 0;
}
```

Output:
```
Hello, my name is James and I am 22 years old.
```
In the above code, values of variables `name` and `age` are interpolated into a string.

## Deep Dive
- Historical Context: Earlier versions of C++ didn't natively support string interpolation. Programmers had to concatenate variables into strings using `+` operator or `sprintf()` method. In C++20, string formatting library `std::format` was introduced making it more convenient.
- Alternatives: There are libraries like `fmt` and `boost::format` that can be used for string formatting. Or you can construct strings using `std::stringstream`.
- Details: C++ does not directly support string interpolation but through methods mentioned above. Unlike Python, Ruby, or modern Javascript, it does not have a built-in template language.

## See Also:
- [String Interpolation in Python](https://docs.python.org/3/tutorial/inputoutput.html#fancier-output-formatting)
- [String Formatting in C++](https://en.cppreference.com/w/cpp/utility/format) 
- [fmt library](https://fmt.dev/latest/)
- [boost::format](https://www.boost.org/doc/libs/1_76_0/libs/format/)
- [std::stringstream](https://en.cppreference.com/w/cpp/io/basic_stringstream)