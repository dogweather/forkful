---
title:                "Перетворення рядка на великі літери"
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Capitalizing a string means transforming all its characters into uppercase. Programmers do it for formatting, user interface consistency, or to ensure case-insensitive comparisons.

## How to: (Як це зробити:)
```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string text = "Привіт, як справи?";
    std::transform(text.begin(), text.end(), text.begin(),
                   [](unsigned char c){ return std::toupper(c); });

    std::cout << text << std::endl; // Виведе: ПРИВІТ, ЯК СПРАВИ?
    return 0;
}
```

## Deep Dive (Занурення вглиб)
In the past, capitalizing a string in C++ meant manually iterating over each character and using functions from the `cctype` library. Today, `<algorithm>` and lambda expressions simplify the process. Alternatives include creating a custom function for transformation or using a third-party library.

The `std::toupper` function works per character and might not handle locale-specific rules perfectly. If you need to handle internationalization properly, consider using a library like ICU (International Components for Unicode) which is designed for such tasks.

Implementation-wise, remember that characters in C++ are ASCII by default. Unicode strings, common in Ukrainian text, need encoding-aware functions to capitalize correctly.

## See Also (Дивіться також)
- C++ `<locale>` library: http://www.cplusplus.com/reference/locale/
- ICU project: http://site.icu-project.org/
- C++ `<algorithm>` library: http://www.cplusplus.com/reference/algorithm/
