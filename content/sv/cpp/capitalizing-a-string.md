---
title:                "Gör om en sträng till versaler"
html_title:           "C++: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att göra en sträng versaler innebär att ändra alla bokstäver i en given sträng till stora bokstäver. Programmerare gör detta ofta för att göra sökningar och matchningar av strängvärden okänsliga för gemener och versaler.

## Hur man gör:
```C++
#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>

void ConvertToUpper(std::string &input_str) {
    std::for_each(input_str.begin(), input_str.end(), [](char &c){
        c = ::toupper(c);
    });
}

int main() {

    std::string test_str = "Hej världen!";
    ConvertToUpper(test_str);
    std::cout << test_str; // Utdata: "HEJ VÄRLDEN!"

    return 0;
}
```
## Djupdykning

Historiskt sett har versalisering av strängar använts för att göra textmatchningar enklare och mer tillförlitliga. Alternativen till denna metod kan vara att använda reguljära uttryck eller andra strängmanipulerande funktioner.

Implementeringen av att konvertera en sträng till versaler i C++ är ganska enkel, tack vare dess rika standardbibliotek. I vår kodbeteckning använder vi `std::for_each` funktionen från `<algorithm>` biblioteket. Den tar en räckvidd (i detta fall början och slutet på vår sträng) och en funktion att tillämpa på varje objekt inom den räckvidden.

Notera att vi använder en lambda funktion som tar varje 'char' i vår sträng och använder `::toupper`.

## Se också

1. C++ referens om för varje: <http://en.cppreference.com/w/cpp/algorithm/for_each>
2. C++ referens om std::toupper: <http://en.cppreference.com/w/cpp/string/byte/toupper>
3. C++ referens om reguljära uttryck: <http://en.cppreference.com/w/cpp/regex>
4. C++ referens om lambda funktionalitet: <http://en.cppreference.com/w/cpp/language/lambda>