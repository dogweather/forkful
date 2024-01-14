---
title:    "C++ recipe: Concatenating strings"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

As a programmer, you may come across situations where you need to combine multiple strings into one. This process is known as concatenation and it is a common task in many programming languages, including C++.

## How To

To concatenate strings in C++, you can use the `+` operator. Let's take a look at an example:

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string firstName = "John";
    string lastName = "Smith";
    string fullName = firstName + " " + lastName;
    cout << "Full name: " << fullName << endl;
    return 0;
}
```

In the above code, we have declared three string variables: `firstName`, `lastName`, and `fullName`. To combine the first and last name, we have used the `+` operator along with the `" "` string literal to add a space between the two names.

The output of this code would be: `Full name: John Smith`

Another important thing to note is that you can also use the `+=` operator to concatenate strings. This operator adds the right operand to the left operand and assigns the result to the left operand. Let's see an example using the same code as before:

```C++
fullName += " Jr.";
```

This will add the string `" Jr."` to the `fullName` variable, resulting in the full name being `John Smith Jr.`.

You can also concatenate string variables with other data types, such as numbers. For example:

```C++
string age = "25";
fullName += ", age " + age;
```

This will add the string `", age 25"` to the `fullName` variable, resulting in the full name being `John Smith Jr., age 25`.

## Deep Dive

Under the hood, when using the `+` operator to concatenate strings, the compiler is actually creating a new string object and copying the contents of the two strings into it. This can be inefficient, especially when concatenating large or multiple strings.

A more efficient way to concatenate strings is by using the `append()` function from the `string` library. This will directly modify the original string instead of creating a new one, resulting in better performance. Here's an example:

```C++
string address = "123 Main Street";
fullName.append(", ").append(address);
```

The output of this code would be: `John Smith Jr., age 25, 123 Main Street`

If you need to concatenate strings in a loop, it is better to use `append()` instead of the `+` operator to avoid creating unnecessary string objects and improve performance.

## See Also

- [C++ Strings Tutorial](https://www.programiz.com/cpp-programming/strings)
- [C++ String Class Reference](https://www.cplusplus.com/reference/string/string/)