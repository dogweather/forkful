---
title:                "Extracting substrings"
html_title:           "C++ recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings is a common task in programming where a certain portion of a larger string is separated out and stored in a separate variable. This can be useful in various situations such as parsing data, manipulating text, or searching for specific patterns within a string. By breaking down a larger string into smaller substrings, programmers can manipulate and organize data in a more efficient manner.

## How to:

Extracting substrings can be easily done in C++ using the `substr()` function which takes in two arguments - the starting index and the length of the desired substring. Here's an example of extracting a substring from a string:

```C++
string name = "John Doe";
string firstName = name.substr(0,4); // extracts substring starting at index 0 with length 4
cout << firstName << endl; // output: John
```

You can also extract substrings from user input using the `cin` function and then manipulate them as needed. For example:

```C++
string sentence;
cout << "Enter a sentence: ";
cin >> sentence;

// extracting a specific word from the sentence
string word = sentence.substr(9,5); // extracts substring starting at index 9 with length 5
cout << word << endl; // assuming input sentence was "Hello World", output: World
```

## Deep Dive:

The `substr()` function was introduced in C++11 and is part of the `<string>` header library. Before this, programmers had to use the `substr()` function from the C library `string.h` which had a different syntax and was not as efficient. Another alternative to `substr()` is using pointers to manipulate string data, but this can be more complex and error-prone.

When extracting a substring, it's important to keep in mind that the index starts at 0 and the length includes the starting character. For example, in the string "Hello World", the index for the letter "W" would be 6 and the length for extracting the word "World" would be 5.

## See Also:

To learn more about manipulating strings in C++, check out the official documentation on `substr()` function: [cplusplus.com/reference/string/string/substr/](https://www.cplusplus.com/reference/string/string/substr/)

You can also explore other string manipulation methods such as `find()`, `replace()`, and `insert()`.

Happy coding!