---
title:                "Använda reguljära uttryck"
html_title:           "Gleam: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Reguljära uttryck, eller regex, används för att matcha specifika mönster i strängar. Denna teknik är mycket användbar för att söka, ersätta, och validera data.

## Hur man:

```C++
// inkludera regex-biblioteket
#include <regex>

int main()
{
    // Skapa ett regex-objekt
    std::regex re("a..a");

    // En teststräng
    std::string str = "abra";

    // Använd 'std::regex_match' för att jämföra strängen med regex-objektet
    if (std::regex_match(str, re)) {
        std::cout << "Matchar!\n";
    } else {
        std::cout << "Matchar inte!\n";
    }

    return 0;
}
```

Programmet skriver ut "Matchar!" eftersom "abra" matchar mönstret "a..a".

## Djupdykning:

Regex skapades i slutet av 60-talet och har sedan dess blivit ett standardverktyg i programmerarens verktygslåda. Alternativ till regex finns. I C++, till exempel, kan man använda str.find() metoden för att göra enklare sökningar i strängar. Men regex ger betydligt mer flexibilitet och kraft.

När det gäller genomförande använder C++ regex-biblioteket ECMAScript-syntak, om inte annat anges. Det ger en bra bas att bygga komplexa mönster. 

## Se Även:

- För ytterligare studier om reguljära uttryck, besök: [C++ Reguljära Uttryck](http://www.cplusplus.com/reference/regex/)