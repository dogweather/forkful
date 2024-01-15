---
title:                "Capitalizing a string"
html_title:           "C++ recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Capitalizing a string might seem like a trivial task, but it can actually have a big impact on the readability and professionalism of your code. By capitalizing strings, you can make your code more consistent and easier to understand for other developers.

## How To

To capitalize a string in C++, there are a few different methods you can use depending on your specific needs. Here are a few examples to get you started:

### Simple Method:
```C++
// Declare a string variable
std::string myString = "hello world";
// Use the toupper function to capitalize the first character
myString[0] = toupper(myString[0]);
// Output: "Hello world"
std::cout << myString << std::endl;
```

### Using String Stream:
```C++
// Declare a string variable
std::string myString ="goodbye world";
// Declare a string stream variable
std::stringstream stream;
// Use the toupper function to capitalize the first character
stream << (char)toupper(myString[0]);
// Output: "Goodbye world"
std::cout << stream.str() << std::endl;
```

### Using Boost Library:
```C++
// Declare a string variable
std::string myString = "hey there";
// Use the boost::algorithm::to_upper_copy function to capitalize all characters
boost::to_upper_copy(myString);
// Output: "HEY THERE"
std::cout << myString << std::endl;
```

## Deep Dive
In C++, strings are represented as an array of characters, making it easy to access individual characters and manipulate them. As seen in the examples above, one way to capitalize a string is by using the toupper function, which takes in a character and returns its uppercase equivalent according to the current locale.

Another method is by using a string stream, which allows for more flexibility in manipulating strings. Meanwhile, the Boost library provides a wide range of string algorithms, including a function to specifically capitalize strings.

It is important to note that these methods only capitalize the first character of a string. In order to capitalize the entire string, you may need to use a loop or apply the method to each word in the string separately.

## See Also
- [std::toupper](https://www.cplusplus.com/reference/cctype/toupper/)
- [std::stringstream](https://www.cplusplus.com/reference/sstream/stringstream/)
- [boost::to_upper_copy](https://www.boost.org/doc/libs/1_74_0/doc/html/boost_algorithm/to_upper_copy_id3691011.html)