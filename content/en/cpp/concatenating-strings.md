---
title:                "Concatenating strings"
html_title:           "C++ recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to combine multiple strings together to create a new string in your C++ program? Maybe you wanted to create a dynamic message or manipulate user input. Whatever the reason, concatenating strings is a useful skill to have in your programming arsenal.

## How To

To concatenate strings in C++, you can use the `+` operator or the `append()` function. Let's take a look at both methods with some examples:

```C++
// Using the + operator
#include <iostream>
#include <string>
using namespace std;

int main() {
  string greeting = "Hello ";
  string name = "John";
  string message = greeting + name;

  cout << message << endl; // Output: Hello John
  return 0;
}

// Using the append() function
#include <iostream>
#include <string>
using namespace std;

int main() {
  string sentence = "I love ";
  string fruit = "apples";
  sentence.append(fruit);

  cout << sentence << endl; // Output: I love apples
  return 0;
}
```

As you can see, both methods result in the same output. The `+` operator is used to directly add two strings together, while the `append()` function adds the second string to the end of the first string.

But what if you want to concatenate more than two strings together? In that case, you can either string together multiple `+` operators or use the `append()` function multiple times.

```C++
// Using multiple + operators
#include <iostream>
#include <string>
using namespace std;

int main() {
  string first_name = "John";
  string last_name = "Doe";
  string full_name = first_name + " " + last_name;

  cout << full_name << endl; // Output: John Doe
  return 0;
}

// Using the append() function multiple times
#include <iostream>
#include <string>
using namespace std;

int main() {
  string sentence = "I love ";
  string fruit = "apples";
  string more = " and oranges";
  sentence.append(fruit).append(more);

  cout << sentence << endl; // Output: I love apples and oranges
  return 0;
}
```

## Deep Dive

When you use the `+` operator to concatenate strings, the compiler actually translates it into a call to the `append()` function. This function takes in a string as its argument and adds it to the end of the original string.

It is also important to note that the `+` operator and `append()` function only work with strings. If you want to concatenate other data types, you will need to use the `stringstream` class. This class allows you to combine different data types into a string.

## See Also

- [C++ String Operators](https://www.programiz.com/cpp-programming/operators)
- [C++ String Class](https://www.geeksforgeeks.org/c-string-class-and-its-applications/)
- [C++ StringStream](http://www.cplusplus.com/reference/sstream/stringstream/)