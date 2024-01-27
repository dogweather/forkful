---
title:                "Att göra en sträng versal"
date:                  2024-01-19
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att göra om en textsträng till enbart versaler innebär att omvandla alla små bokstäver till stora. Programmerare gör detta för att standardisera data, förbättra användarupplevelse eller göra texten mer synlig.

## Hur gör man:
```C++
#include <iostream>
#include <string>
#include <algorithm>

// Funktion för att omvandla en sträng till versaler
std::string to_uppercase(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(), ::toupper);
    return s;
}

int main() {
    std::string text = "Hej, jag är en sträng!";
    std::string uppercase_text = to_uppercase(text);
    std::cout << uppercase_text << std::endl; // Output: "HEJ, JAG ÄR EN STRÄNG!"
    return 0;
}
```

## Deep Dive
Funktionen `std::transform` är en del av standardbiblioteket sedan C++98 och används för att tillämpa en operation på en sekvens. Här använder vi den med `::toupper`, som konverterar en enskild tecken till versal. 

Ett alternativ är att använda en range-based for-loop och uppdatera varje tecken manuellt. Det blev dock omständligare och `std::transform` är att föredra för sin enkelhet och effektivitet.

Förr i tiden kunde stora datamängder vara svåra att bearbeta på grund av begränsad processorkraft och minne. Nu är operationer som strängmanipulation mer överkomliga, men att skriva effektiv kod är fortfarande viktigt för systemets prestanda. 

## Se även
- [C++ reference for `std::transform`](https://en.cppreference.com/w/cpp/algorithm/transform)
- [C++ reference for `toupper`](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [C++ Standard Library](https://www.iso.org/standard/79358.html)
