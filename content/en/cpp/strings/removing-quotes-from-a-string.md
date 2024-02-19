---
aliases:
- /en/cpp/removing-quotes-from-a-string/
date: 2024-01-25 20:50:01.125261-07:00
description: "Stripping quotes from a string means peeling away those pesky double\
  \ or single characters that encase our text (' or \"). Programmers often do this\
  \ to\u2026"
lastmod: 2024-02-18 23:09:11.343751
model: gpt-4-1106-preview
summary: "Stripping quotes from a string means peeling away those pesky double or\
  \ single characters that encase our text (' or \"). Programmers often do this to\u2026"
title: Removing quotes from a string
---

{{< edit_this_page >}}

## What & Why?
Stripping quotes from a string means peeling away those pesky double or single characters that encase our text (' or "). Programmers often do this to sanitize input, store text in a database, or prep strings for further processing without the clutter of quotation marks.

## How to:
Here's a straightforward way to kick those quotes to the curb in C++:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hello, 'World'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

Run this, and you'll get:

```
Hello, World!
```

Voila! The quotes have vanished.

## Deep Dive
Quotations have been a text nuisance since the dawn of computing. Back in the day, you'd see programmers laboriously loop through each character to filter out those quotes. Today, we've got `std::remove` in the Standard Template Library (STL) to do the heavy lifting.

Alternatives? Sure! You could use regular expressions with `std::regex` to target quotes, but that's a bit like using a sledgehammer to crack a nut - powerful, but can be overkill for simple tasks. For those favoring recent C++ flavors, you might dabble with `std::string_view` for non-modifying approaches.

Implementation wise, remember that `std::remove` doesn't actually remove elements from the container; it shuffles non-removed elements forward and returns an iterator past the new end of the range. That's why we need the `erase` method to chop off the unwanted tail.

## See Also
- C++ `std::remove` reference: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- More on `std::string` manipulation: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
