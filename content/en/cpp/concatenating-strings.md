---
title:    "C++ recipe: Concatenating strings"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why

Concatenating strings, or combining multiple strings into one, is a common task in programming. It allows us to dynamically create new strings based on existing ones, making our code more flexible and efficient. In this blog post, we will discuss why and how to concatenate strings in C++.

## How To

To concatenate strings in C++, we can use the `+` operator or the `append()` function. Let's take a look at some examples using both methods:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {

    // Concatenating using the "+" operator
    string first_name = "John";
    string last_name = "Doe";
    string full_name = first_name + " " + last_name;
    cout << "Full name: " << full_name << endl;

    // Concatenating using the "append()" function
    string message = "Hello ";
    string name = "Mary";
    message.append(name);
    cout << message << endl;

    return 0;
}
```

Output:

```
Full name: John Doe
Hello Mary
```

We can also concatenate strings using the `+=` operator, which adds the second string to the end of the first string:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {

    string sentence = "This is a ";
    sentence += "sentence.";
    cout << sentence << endl;

    return 0;
}
```

Output:

```
This is a sentence.
```

## Deep Dive

Concatenating strings may seem like a simple task, but there are some important things to keep in mind. One thing to note is that strings are immutable in C++, meaning that once a string is created, it cannot be modified. This is why each concatenation operation results in a new string being created.

It is also important to be mindful of the performance implications of concatenating strings. Using the `+` operator can be less efficient compared to the `append()` function, especially when concatenating multiple strings. This is because the `+` operator may create temporary strings, which can affect the overall performance of our code.

To avoid these performance issues, we can use the `append()` function with `reserve()` to allocate a specific amount of memory for the new string. This can improve the performance of our code when dealing with large strings or multiple concatenations.

## See Also

- [C++ Strings](https://www.geeksforgeeks.org/cpp-strings/)
- [C++ String Concatenation](https://www.programiz.com/cpp-programming/string-concatenation)
- [C++ String Performance Tips](https://www.fluentcpp.com/2017/12/05/basic-string-concatenation-performance-problem-in-c/)