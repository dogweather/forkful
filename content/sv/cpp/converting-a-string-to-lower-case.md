---
title:                "Omvandla en sträng till små bokstäver"
html_title:           "C++: Omvandla en sträng till små bokstäver"
simple_title:         "Omvandla en sträng till små bokstäver"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Konvertering av en sträng till gemener är en grundläggande operation inom programmering som innebär att alla bokstäver i en sträng ändras till små bokstäver. Detta görs vanligtvis för att underlätta jämförelser och sökningar av strängar, eftersom gemener och versaler inte är samma tecken för datorn.

## Hur man:
Här är ett exempel på hur man konverterar en sträng till gemener i C++:

```C++
#include <iostream>
#include <algorithm>

using namespace std;

int main() {
  string str = "HELLO WORLD";
  transform(str.begin(), str.end(), str.begin(), ::tolower);
  cout << str; // hej värld
  return 0;
}
```
Här använder vi standardfunktionen `transform` tillsammans med `::tolower` för att ändra alla bokstäver i strängen `str` till små bokstäver.

## Djupdykning:
I äldre versioner av C++, innan standardbiblioteket `algorithm` fanns tillgängligt, användes ofta en loop för att konvertera strängen till gemener. Detta krävde mer kod och ökade risken för fel. Nu med standardfunktionen `transform` ges en mer effektiv och säkrare lösning.

Det finns också andra alternativ för att utföra samma operation, som att använda andra standardfunktioner eller till och med skriva en egen funktion. Det viktiga är att välja den lösning som passar bäst för ens specifika program och behov.

När det gäller implementationen av konverteringen av strängar till gemener är det ofta enkelt och snabbt att utföra. I de flesta fall är det en linjär operation, vilket innebär att det tar lika många steg som antalet bokstäver i strängen.

## Se även:
Mer om funktionen `transform` och andra standardfunktioner: [C++ Standard Library](https://en.cppreference.com/w/cpp/header/algorithm)

En alternativ lösning med en egen funktion: [Lowercase Function](https://www.geeksforgeeks.org/conversion-whole-string-uppercase-lowercase-using-stl-c/)