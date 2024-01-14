---
title:                "C++ recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to convert a string to lower case in your C++ program? Maybe you want to standardize user input or compare strings without worrying about capitalization. Whatever the reason, knowing how to convert a string to lower case can be a useful skill in any C++ programmer's toolbox.

## How To

Converting a string to lower case in C++ is a fairly straightforward process. We will be using the `std::transform()` function from the <algorithm> library to achieve this. Let's take a look at how we can implement this in our code:

```C++
std::string str = "Hello, WORLD!";

// Using std::transform() with a lambda function
std::transform(str.begin(), str.end(), str.begin(),
    [](unsigned char c) { return std::tolower(c); });

std::cout << str << std::endl;
```

The `std::transform()` function takes in three parameters - the beginning and ending iterators of the string, and a function that will be applied to each character in the string. In this case, we are using a lambda function to convert each character to its lowercase version using the `std::tolower()` function.

The output of the above code would be: `hello, world!` since all the characters in the string have been converted to lowercase. We can also use a range-based for loop to achieve the same result:

```C++
std::string str = "Hello, WORLD!";

// Using a range-based for loop
for (auto& c : str) {
    c = std::tolower(c);
}

std::cout << str << std::endl;
```

Both methods produce the same output. It is important to note that these methods will only work for ASCII characters and may give unpredictable results for non-ASCII characters.

## Deep Dive

C++ does not have a built-in function to convert a string to lower case, unlike some other programming languages like Python. This is why we rely on the `std::transform()` function to help us achieve the desired result.

The `std::toupper()` and `std::tolower()` functions are part of the <cctype> library and take in a single character as a parameter and return its uppercase or lowercase equivalent, respectively. These functions only work with ASCII characters and may not work as expected for non-ASCII characters.

Additionally, it is important to note that both of these methods will modify the original string. If you do not want to alter the original string, you can create a copy of the string and convert it to lower case using the same methods shown above.

## See Also

- [C++ String Reference](https://www.cplusplus.com/reference/string/)
- [std::transform() documentation](https://www.cplusplus.com/reference/algorithm/transform/)
- [std::toupper() documentation](https://www.cplusplus.com/reference/cctype/toupper/)
- [std::tolower() documentation](https://www.cplusplus.com/reference/cctype/tolower/)