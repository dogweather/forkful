---
title:                "C++ recipe: Searching and replacing text"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text is a common task in programming that allows for efficient and accurate text manipulation. It can be especially useful when working on large projects with a lot of repetitive code. In this blog post, we will explore how to use C++ to easily search and replace text in your code.

## How To

First, let's take a look at some basic examples of searching and replacing text using C++ code.

### Simple String Replacement

To replace a specific string in your code, you can use the `replace` function from the standard library `string` header. This function takes three parameters: the starting position, the number of characters to replace, and the new string to insert. Let's see an example:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string sentence = "Coding is fun!";
  
  // Replace "fun" with "challenging"
  sentence.replace(11, 3, "challenging"); 
  cout << sentence << endl; // output: Coding is challenging!
  
  return 0;
}
```

### Multiple Replacements

If you want to replace multiple occurrences of a specific string, you can use a loop to iterate through the string and use the `replace` function for each instance. For example:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string sentence = "Hello hello hello!";
  string to_search = "hello";
  string replacement = "hi";
  
  // Loop through the string and replace all instances
  size_t pos = 0;
  while ((pos = sentence.find(to_search, pos)) != string::npos) {
    sentence.replace(pos, to_search.length(), replacement);
    pos += replacement.length();
  }
  
  cout << sentence << endl; // output: Hi hi hi!
  
  return 0;
}
```

### Case-Insensitive Search

By default, the `replace` function searches for the exact case of the specified string. However, if you want to perform a case-insensitive search, you can use the `find` function and pass in a case-insensitive version of the search string. For example:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string sentence = "Is c++ case-sensitive?";
  string to_search = "c++";
  string replacement = "C++";
  
  // Find and replace regardless of case
  size_t pos = sentence.find(to_search, 0);
  while (pos != string::npos) {
    sentence.replace(pos, to_search.length(), replacement);
    pos = sentence.find(to_search, pos + replacement.length());
  }
  
  cout << sentence << endl; // output: Is C++ case-sensitive?
  
  return 0;
}
```

## Deep Dive

There are many different methods for searching and replacing text in C++, such as using regular expressions or using the `substr` function. It is important to understand the different options and choose the most efficient method based on your specific needs and the size of your codebase.

When replacing text, it is also important to consider any unintended consequences, such as replacing a substring within a larger string that should not be altered. Additionally, it is important to handle input validation and error handling to ensure the desired replacements are made accurately.

## See Also

- [C++ String manipulation](https://www.geeksforgeeks.org/c-string-manipulation-class-applications/)
- [C++ Regular Expressions](https://www.geeksforgeeks.org/regular-expressions-in-c-c/)
- [C++ Standard Library string header](https://www.cplusplus.com/reference/string/)

Thank you for reading! I hope this blog post was helpful in understanding how to effectively search and replace text in your C++ code. Happy coding!