---
aliases:
- /en/cpp/converting-a-string-to-lower-case/
date: 2024-01-20 17:38:15.389607-07:00
description: "Converting a string to lower case means transforming all uppercase letters\
  \ to their lowercase equivalents. Programmers do this for consistency in user\u2026"
lastmod: 2024-02-18 23:09:11.342797
model: gpt-4-1106-preview
summary: "Converting a string to lower case means transforming all uppercase letters\
  \ to their lowercase equivalents. Programmers do this for consistency in user\u2026"
title: Converting a string to lower case
---

{{< edit_this_page >}}

## What & Why?
Converting a string to lower case means transforming all uppercase letters to their lowercase equivalents. Programmers do this for consistency in user inputs, data processing, and to simplify text comparisons.

## How to:
Here's how you smash case differences in C++, capital letters bowing down to the little ones:

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string origText = "C++ makes me Shout!";
    std::string lowerText = origText;

    std::transform(origText.begin(), origText.end(), lowerText.begin(), 
                   [](unsigned char c) { return std::tolower(c); });

    std::cout << "Original: " << origText << std::endl;
    std::cout << "Lowercase: " << lowerText << std::endl;
    
    return 0;
}
```
Output:
```
Original: C++ makes me Shout!
Lowercase: c++ makes me shout!
```

## Deep Dive
Back in the day, before `std::transform` and lambdas entered the scene, one would loop through each character and lower it manually—a tad more grunt work. `std::transform` with `std::tolower` is efficient and less error-prone, though, knowing C++, other ways exist. Mind the locale: `std::tolower`’s behavior can vary. If your project screams Unicode, check out third-party libraries like ICU that are built for a global stage.

It’s also worth mentioning C++20's addition, `std::ranges::transform`, which brings in range-based transformations, spicing up the syntax and adhering to the 'range' philosophy that coding should be more intuitive and less prone to errors.

As for implementation details, each character has an ASCII value, and the difference between lowercase and uppercase is consistent. Transformations peep these values to lower them—basically playing numerical limbo.

## See Also
For those curious cats who are hungry for more:

- C++ reference for `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
- C++ reference for `std::tolower`: https://en.cppreference.com/w/cpp/string/byte/tolower
- Details about C++20's `std::ranges`: https://en.cppreference.com/w/cpp/ranges

Craving Unicode understanding? Try the ICU Project:
- ICU Project: http://site.icu-project.org/home
