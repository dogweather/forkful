---
title:    "C++ recipe: Capitalizing a string"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why

Capitalizing a string is a common task in programming, especially when working with user input or manipulating data. By converting the string to all upper case or all lower case, it makes it easier to compare and manipulate strings in an efficient manner. This can be especially useful in tasks such as sorting or searching for specific data within a string.

## How To

To begin, let's define a string variable that we want to capitalize:

```C++
string str = "hello world";
```

To convert this string to all uppercase, we can use the `toupper()` function from the `<cctype>` library. This function takes in a single character as an argument and returns the uppercase version of that character. We can loop through the characters in our string and pass each one to the `toupper()` function to convert them to uppercase. Here's an example:

```C++
for (int i = 0; i < str.length(); i++) {
  str[i] = toupper(str[i]);
}

cout << str << endl;
```

The output of this code would be "HELLO WORLD".

To convert a string to all lowercase, we can use the `tolower()` function in a similar manner to the `toupper()` function. Here's an example:

```C++
for (int i = 0; i < str.length(); i++) {
  str[i] = tolower(str[i]);
}

cout << str << endl;
```

The output of this code would be "hello world".

## Deep Dive

There are a few things to keep in mind when capitalizing a string. Firstly, it's important to remember that the `toupper()` and `tolower()` functions only work on single characters. So when looping through a string, we need to access each character using square brackets, `[]`, and pass it to the function.

Secondly, these functions work differently for different character sets. For example, if our string contains special characters or symbols, the result of converting them to uppercase or lowercase may not be what we expected. This is because different character sets have different mappings for upper and lower case letters.

Lastly, it's important to consider the efficiency of our code when capitalizing a string. In the examples above, we are modifying the original string in place by using the assignment operator, `=`, to replace each character in the string with its uppercase or lowercase equivalent. However, this may not be the most efficient approach if we need to retain the original string as well. In such cases, we can create a new string and copy over the converted characters, leaving the original string unchanged.

## See Also

- [toupper() function in C++](https://www.cplusplus.com/reference/cctype/toupper/)

- [tolower() function in C++](https://www.cplusplus.com/reference/cctype/tolower/)