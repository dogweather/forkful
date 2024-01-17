---
title:                "Användning av reguljära uttryck"
html_title:           "C++: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Reguljära uttryck är ett kraftfullt verktyg för att söka och bearbeta textsträngar i dina C++ program. Det låter dig definiera mönster som matchar en viss uppsättning tecken eller ord, vilket kan användas för att hitta, extrahera eller ersätta text i en sträng. Programmerare använder reguljära uttryck för att effektivt hantera textsträngar och utföra olika operationer på dem.

## Hur man gör:

Så här kan du använda reguljära uttryck i dina C++ program:

```C++
#include <iostream>
#include <regex>

int main() {
  std::string text = "Detta är en text som innehåller ordet programmering.";
  std::regex pattern("programmering");

  if (std::regex_search(text, pattern)) {
    std::cout << "Hittade ordet \"programmering\" i texten!" << std::endl;
  }

  return 0;
}
```

Detta program kommer att söka efter mönsteret "programmering" i texten och skriva ut "Hittade ordet \"programmering\" i texten!" om det finns en matchning.

## Djupdykning:

Reguljära uttryck har funnits sedan 1950-talet och är ett viktigt verktyg för textbehandling i många programmeringsspråk. I C++ kan du använda standardbibliotekets <regex> för att arbeta med reguljära uttryck.

En alternativ metod för att hantera textsträngar är att använda strängfunktioner som finner, ersätter eller manipulerar delar av en sträng baserat på en given position eller index.

## Se även:

Här är några användbara länkar för att lära dig mer om reguljära uttryck i C++:

- C++ Referens om reguljära uttryck: http://www.cplusplus.com/reference/regex/
- RegExr - en online verktyg för att testa och utforska reguljära uttryck: https://regexr.com/
- Utförlig guide till reguljära uttryck i C++: https://www.geeksforgeeks.org/regular-expressions-in-c-c/