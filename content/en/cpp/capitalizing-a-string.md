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

## What & Why?

Capitalizing a string means converting all the alphabetic characters in the string to uppercase. Programmers do it to make the text unambiguous, or to meet certain standards (e.g., usernames, file formats).

## How to:

Here's a simple way to capitalize a string using C++:

```C++
#include <algorithm>
#include <cctype>
#include <string>
...
std::string stringToCapitalize = "c++ programming";
std::transform(stringToCapitalize.begin(), stringToCapitalize.end(), stringToCapitalize.begin(), ::toupper);

std::cout << stringToCapitalize << std::endl;
```
After running this code, the output should be:
```
C++ PROGRAMMING
```

## Deep Dive

Historically, ASCII-based systems used capital letters because they were easier to read on primitive displays. These days, we like to use lowercase more often, but capitalization is still handy for standardization and ease of reading.

Alternatives to the transform method can occasionally be more efficient. For example, using a for-loop and the toupper function, like this:

```C++
std::string s = "c++ programming";
for(char &c : s)
    c = toupper(c);
```

The actual capitalization happens in the `::toupper` function. This is part of the C++ Standard Library, inherited from the C language. It translates each alphabetical character into its uppercase equivalent according to the current locale. Non-alphabetical characters remain unchanged.

## See Also

Beyond this intro, you'll want to explore these resources:
- [cppreference - std::transform](https://en.cppreference.com/w/cpp/algorithm/transform) for more ways you can leverage the transform function.
- [cplusplus - toupper](http://www.cplusplus.com/reference/cctype/toupper/) for a deeper dive into the toupper function.
- [StackOverflow questions tagged with 'uppercase'](https://stackoverflow.com/questions/tagged/uppercase) for practical issues and solutions shared by other programmers.