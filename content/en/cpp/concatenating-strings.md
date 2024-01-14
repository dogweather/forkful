---
title:                "C++ recipe: Concatenating strings"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
When writing a program in C++, there may come a time when you need to combine multiple strings into one larger string. This process, known as concatenation, is essential for creating dynamic and customizable output in your code.

## How To
To concatenate strings in C++, you can use the `+` operator as shown in the following code snippet:

```C++
#include <iostream>
using namespace std;

int main()
{
    //declaring and initializing two strings
    string name = "John";
    string greeting = "Hello, ";

    //concatenating the strings using the + operator
    string message = greeting + name;

    //printing the concatenated string
    cout << message << endl;

    return 0;
}
```
Output:
```
Hello, John
```
By using the `+` operator, you can easily combine strings of any length within your code. It's important to note that you can also concatenate strings that are not stored in variables, as shown in the following example:

```C++
#include <iostream>
using namespace std;

int main()
{
    //concatenating strings without variables
    string message = "I" + " love" + " coding.";

    //printing the concatenated string
    cout << message << endl;

    return 0;
}
```
Output:
```
I love coding.
```

## Deep Dive
Under the hood, when you use the `+` operator to concatenate strings, you are actually creating a new string object. This can be a bit inefficient, especially if you are concatenating multiple strings within a loop. To improve performance, you can use the `append()` function, which adds the contents of one string to the end of another string.

For example, the following code uses `append()` to concatenate the same strings as the first example:

```C++
#include <iostream>
using namespace std;

int main()
{
    //declaring and initializing two strings
    string name = "John";
    string greeting = "Hello, ";

    //concatenating the strings using the append() function
    greeting.append(name);

    //printing the concatenated string
    cout << greeting << endl;

    return 0;
}
```
Output:
```
Hello, John
```
This approach is more efficient because it appends the string to the existing one rather than creating a new object every time.

Another thing to keep in mind when concatenating strings is the use of whitespace. If you want to add a space between two concatenated strings, you can simply include it within the quotations, as shown in the first example. However, if you want to add a different character or no character at all, you can use the `+` or `append()` functions on a single character or string.

## See Also
- [C++ Strings](https://www.programiz.com/cpp-programming/strings)
- [C++ Operators](https://www.tutorialspoint.com/cplusplus/cpp_operators.htm)
- [C++ String append function](https://www.geeksforgeeks.org/c-string-append-function/)