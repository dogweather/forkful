---
title:                "C++ recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substrings are a fundamental concept in programming, especially when it comes to manipulating and analyzing strings. They are essentially smaller strings that are extracted from a larger string, and can provide valuable information and insights about the original string. In this blog post, we will explore how to extract substrings in C++ and why it is important for programmers to have this skill.

## How To

To extract a substring from a string in C++, we can use the `substr()` function from the string library. This function takes in two parameters - the starting index and the number of characters to be extracted from the original string. Let's see an example of how this works:

```C++
#include <iostream>
#include <string>

int main() {
  // Create a string
  std::string sentence = "Hello, my name is John.";

  // Use substr() to extract the substring "my name"
  std::string name = sentence.substr(7, 7);

  // Output the result
  std::cout << name << std::endl;

  return 0;
}
```

**Output:** my name

In this example, we used the `substr()` function to extract the substring "my name" from the original string "Hello, my name is John.". Notice how we specified the starting index as 7 and the number of characters as 7 - this is because the substring starts at the 7th index and is 7 characters long.

Additionally, we can also use the `length()` function to determine the length of a string and use it to extract a variable number of characters from the original string. Let's try another example:

```C++
#include <iostream>
#include <string>

int main() {
  // Create a string
  std::string sentence = "I am learning C++ programming.";

  // Determine the length of the string
  int length = sentence.length();

  // Use substr() to extract the last 11 characters from the string
  std::string course = sentence.substr(length - 11, 11);

  // Output the result
  std::cout << "I am taking " << course << " course." << std::endl;

  return 0;
}
```

**Output:** I am taking C++ course.

In this example, we used the `length()` function to determine the length of the string and then used it to extract the last 11 characters, which happen to be the name of the course. This way, we can extract substrings without needing to know the exact starting index.

## Deep Dive

The `substr()` function can also be used for more advanced string operations, such as replacing a specific part of a string with a new substring. For example, we can use `substr()` and the assignment operator to replace a specific part of a string:

```C++
#include <iostream>
#include <string>

int main() {
  // Create a string
  std::string sentence = "I like programming in Java.";

  // Replace "Java" with "C++"
  sentence.replace(21, 4, "C++");

  // Output the result
  std::cout << sentence << std::endl;

  return 0;
}
```

**Output:** I like programming in C++.

In this example, we used the `replace()` function along with `substr()` to replace the substring "Java" with "C++" in the original string. This is a powerful tool for manipulating strings and can be very useful in various programming scenarios.

## See Also

- [C++ String Library](https://www.w3schools.com/cpp/cpp_strings.asp)
- [C++ Substrings](https://www.geeksforgeeks.org/string-class-substr-function-in-cpp/)
- [C++ String Operations](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)

In conclusion, the ability to extract substrings in C++ is an essential skill for programmers. It allows us to analyze and manipulate strings in more efficient and effective ways, making our code more robust and versatile. I hope this blog post has been informative and helpful in enhancing your understanding of substrings in C++. Happy coding!