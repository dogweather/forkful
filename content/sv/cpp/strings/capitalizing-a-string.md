---
title:                "Gör om en sträng till versaler"
aliases:
- /sv/cpp/capitalizing-a-string.md
date:                  2024-02-03T19:05:14.842233-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gör om en sträng till versaler"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad och varför?
Att göra en strängs första bokstav i varje ord versal handlar om att konvertera den initiala karaktären av varje ord i strängen till versal om den är i gemen, samtidigt som de återstående karaktärerna förblir oförändrade. Programmerare utför ofta denna uppgift för att formatera utdata, användarinmatningar eller databehandling för att säkerställa konsekvens i hur text presenteras eller bearbetas, särskilt i användargränssnitt eller uppgifter för datanormalisering.

## Hur man gör:
I C++ kan du göra en strängs första bokstav i varje ord versal med hjälp av standardbiblioteket utan att behöva använda dig av tredjepartsbibliotek. För mer komplexa eller specifika beteenden för versalisering kan dock bibliotek som Boost vara mycket användbara. Nedan visas exempel som illustrerar båda tillvägagångssätten.

### Använda C++ Standardbiblioteket:

```cpp
#include <iostream>
#include <cctype> // för std::tolower och std::toupper
#include <string>

std::string capitalizeString(const std::string& input) {
    std::string result;
    bool capitalizeNext = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        }
        result += ch;
    }

    return result;
}

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // Utdata: "Hello World From C++"
}
```

### Använda Boost-biblioteket:

För mer avancerad strängmanipulering, inklusive lokal medveten versalisering, kanske du vill använda Boost String Algo-biblioteket.

Se till att du först har Boost-biblioteket installerat och konfigurerat i ditt projekt. Sedan kan du inkludera de nödvändiga rubrikfilerna och använda dess funktioner som visas nedan.

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // sätt första bokstaven i varje ord till versal
    boost::algorithm::to_lower(capitalizedText); // säkerställer att strängen är i gemen
    capitalizedText[0] = std::toupper(capitalizedText[0]); // gör första tecknet till en versal

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // gör till versal efter ett mellanslag
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // Utdata: "Hello World From C++"
}
```

I detta fall förenklar Boost vissa av uppgifterna för strängmanipulering men kräver fortfarande ett anpassat tillvägagångssätt för verklig versalisering eftersom det främst erbjuder transformations- och gemener till versaler-omvandlingsverktyg.
