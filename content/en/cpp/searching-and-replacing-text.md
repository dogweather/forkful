---
title:                "Searching and replacing text"
html_title:           "C++ recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Text is an essential part of programming, and sometimes we need to make changes to it. This could be due to a variety of reasons, such as fixing errors, updating information, or improving the overall readability of our code. Searching and replacing text is a convenient way to make these changes efficiently and accurately.

## How To
To search and replace text in C++, we can use the `replace` function from the standard library's `string` class. Here is an example code block showing how we can use this function to replace a specific word in a string:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string greeting = "Hello, world!";
  greeting.replace(greeting.find("world"), 5, "C++");
  
  cout << greeting << endl;
  
  return 0;
}
```
Output:
```
Hello, C++!
```

In this code, we first declare a `string` variable called `greeting` and assign it the value "Hello, world!". Then, using the `find` function, we can determine the starting index of the word "world" in the string. This index is then passed to the `replace` function along with the number of characters to be replaced (in this case, 5) and the new text we want to insert ("C++"). Finally, we print the updated `greeting` string to see the result.

## Deep Dive
The `replace` function goes beyond just simple search and replace actions. It also allows us to specify a starting position and the number of characters to be replaced. This means we can replace multiple occurrences of a word or phrase within a string. Additionally, we can even use `replace` to insert new text into a string without replacing anything. Furthermore, we can also use the `replace` function to swap portions of a string by specifying the same starting and ending positions.

Another useful function for searching and replacing text in C++ is `substr`. This function allows us to extract a portion of a string based on a starting index and the number of characters to be extracted. By combining `substr` with `replace`, we can easily manipulate different parts of a string without having to create a new string variable.

In summary, searching and replacing text in C++ is a versatile and powerful tool for managing and manipulating text within our code.

## See Also
- [C++ strings and their properties](https://www.programiz.com/cpp-programming/string)
- [The C++ Standard Library](https://www.learncpp.com/cpp-tutorial/the-standard-c-library/)
- [C++ String Replace Reference](https://en.cppreference.com/w/cpp/string/basic_string/replace)