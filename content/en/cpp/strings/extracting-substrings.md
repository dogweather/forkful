---
title:                "Extracting substrings"
aliases:
- /en/cpp/extracting-substrings.md
date:                  2024-01-20T17:45:06.166590-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracting substrings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings means snatching little pieces out of a larger string. Programmers do this to isolate, process, or analyze specific data within text, like pulling out usernames from email addresses or dates from logs.

## How to:

C++ makes it easy to grab a substring. `std::string` is our trusty sidekick here, with the `substr()` function doing most of the heavy lifting. Let's cut to the chase with some code:

```C++
#include <iostream>
#include <string>

int main() {
    std::string fullString = "Hello, World! Programming in C++ is fun.";
    std::string snippet;

    // Extract "World" starting at index 7 with length 5
    snippet = fullString.substr(7, 5);
    std::cout << snippet << std::endl; // Output: World

    // Extract "Programming" starting at index 14
    snippet = fullString.substr(14);
    std::cout << snippet << std::endl; // Output: Programming in C++ is fun.

    return 0;
}
```

## Deep Dive

Substrings aren't new. Old-school C programmers used `strncpy` and manual bookkeeping. String handling's a common breed of bugs, so C++ aimed to simplify it. `std::string` and its `substr` method date back to C++98 and have been relieving stress since.

Alternatives? Sure. You could go manual with `std::string::iterator` or dust off C functions—if you like living dangerously. A more modern take might involve string_views for non-modifying peeking.

Implementation? Under the hood, `substr` often allocates new storage and copies data, which isn't free. It's light compared to wrestling with raw pointers and char arrays of ye olde times, but it's not instant.

## See Also

For more on `std::string` and its buddies:
- cppreference.com on `std::string`: https://en.cppreference.com/w/cpp/string/basic_string
- More on `std::string_view`: https://en.cppreference.com/w/cpp/string/basic_string_view
- C-style string handling (for historical kicks): http://www.cplusplus.com/reference/cstring/
