---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"

category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? / Vad & Varför?
Regular expressions är textmönster som gör det smidigt att matcha, söka och hantera strängar. Programmerare använder det för att effektivisera och förenkla string manipulering och data validering.

## How to: / Hur man gör:
```C++
#include <iostream>
#include <regex>

int main() {
    std::string input = "Koda med stil i C++ 2023!";
    std::regex reg("(\\bC\\+\\+\\b)");
    
    // Kollar om input matchar regular expression
    if (std::regex_search(input, reg)) {
        std::cout << "Match hittades!" << std::endl;
    } else {
        std::cout << "Ingen matchning." << std::endl;
    }

    // Exempel på att ersätta text
    std::string nyStr = std::regex_replace(input, reg, "C#");
    std::cout << nyStr << std::endl;  // Output: Koda med stil i C# 2023!

    return 0;
}
```

## Deep Dive / Djupdykning:
Regular expressions, eller regex, har anor från 1950-talet men blev populärt genom Perl under 80-talet. Alternativ till regex inkluderar parsing libraries eller manuell strängmanipulering, men de kan vara mer tidskrävande. C++ använder `<regex>` biblioteket från C++11 och framåt där `std::regex` är centrala klassen för hantering av regular expressions.

## See Also / Se även:
- C++ Standards Committee papers: https://isocpp.org/std/the-standard
- Regex101, för att testa regex online: https://regex101.com
- Cppreference - Regular expressions: https://en.cppreference.com/w/cpp/regex
