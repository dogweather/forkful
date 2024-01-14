---
title:    "C++ recipe: Extracting substrings"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why Extract Substrings?

Substrings are smaller pieces of a larger string that can be extracted for use in various operations. This allows for more efficient and specific data manipulation, making substring extraction a valuable tool in programming.

## How To Extract Substrings

To extract a substring in C++, we will be using the `substr()` function from the `<string>` library. This function takes in two parameters: the starting index and the number of characters to extract.

Let's take a look at an example:

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
  string fullString = "Hello World";
  string subString = fullString.substr(3, 5);
  cout << subString << endl;
  return 0;
}
```

The output of this code would be `lo Wo`, as we starting extracting at index 3 and extracted 5 characters.

## Deep Dive into Substring Extraction

It's important to note that the starting index of the substring extraction is inclusive, meaning the character at that index is included in the substring. However, the number of characters to extract is exclusive, meaning the character at that index will not be included in the substring.

We can also extract substrings from a specific starting index to the end of the string without specifying the number of characters to extract. Let's see an example:

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
  string fullString = "Hello World";
  string subString = fullString.substr(6);
  cout << subString << endl;
  return 0;
}
```

The output of this code would be `World`, as we started extracting at index 6 and did not specify the number of characters to extract.

## See Also

- [C++ Substring Extraction Tutorial](https://www.programiz.com/cpp-programming/library-function/string/substr)
- [String Class in C++](https://www.geeksforgeeks.org/string-class-in-c/)
- [C++ String Functions](https://www.tutorialspoint.com/cpp_standard_library/cpp_string_functions.htm)