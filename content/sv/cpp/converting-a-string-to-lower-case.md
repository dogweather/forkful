---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener innebär att ändra alla stora bokstäver i en textsträng till små bokstäver. Programmerare gör det ofta för att förhindra oönskad variation beroende på versaler och gemener i sökningar, sorteringar, autentiering och andra jämförelser.

## Så här gör du:
I C++ kan du enkelt konvertera en sträng till gemener med STL algoritmen `std::transform`. Här är ett exempel:

```C++
#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>

int main() {
    std::string text = "Hej Världen!";
    std::transform(text.begin(), text.end(), text.begin(),
                   [](unsigned char c){ return std::tolower(c); });
    std::cout << text;
    return 0;
}
```
När du kör detta program får du "hej världen!" som output.


## Fördjupning
Historiskt sätt har konvertering av strängar till gemener varit nödvändig för att förenkla textbehandling och jämförelser inom datakommunikation. I äldre programmeringsspråk som C skulle det vara mer komplicerat att utföra en sådan konvertering, men med C++ och standardmallsbiblioteket är det en ganska rak framåt process.

Alternativa metoder inkluderar att skriva en egen funktion för att iterera genom varje tecken i strängen och använda funktionen `tolower()` från `cctype`-biblioteket för att konvertera varje tecken individuellt.

Om du behöver konvertera tecken i andra teckensystem än ASCII, som Unicode, skulle du behöva använda mer avancerade metoder där `std::tolower` kanske inte fungerar. Detta är eftersom `std::tolower` endast arbetar med ASCII-tecken och inte kan hantera andra teckensystem.

## Se även
För att lära dig mer om strängbehandling i C++, kolla in dessa resurser:
1. [Basic string manipulations in C++](https://www.cplusplus.com/reference/string/string/)
2. [C++ String Transformation](https://en.cppreference.com/w/cpp/algorithm/transform)
3. [Character conversions in C++](https://www.cplusplus.com/reference/cctype/tolower/)