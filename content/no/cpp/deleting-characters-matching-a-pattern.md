---
title:                "Sletting av tegn som matcher et mønster."
html_title:           "C++: Sletting av tegn som matcher et mønster."
simple_title:         "Sletting av tegn som matcher et mønster."
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Sletting av tegn som matcher et mønster er nyttig når du vil fjerne uønskede tegn fra en streng. Dette kan være nyttig når du jobber med tekstbehandling og ønsker å rense opp i dataene dine.

## Slik gjør du det

```C++
#include <iostream>
#include <string>

using namespace std;

int main()
{
    // Opprett en streng med uønskede tegn
    string streng = "H-e-$l-l-o";
    
    // Opprett et mønster ved hjelp av et regex-uttrykk
    regex mønster("[^a-zA-Z]"); 

    // Erstatt alle tegn som matcher mønsteret med et blankt tegn
    string rensetStreng = regex_replace(streng, mønster, "");
    
    // Skriv ut den rensete strengen
    cout << rensetStreng << endl;
    
    return 0;
}
```

**Output:**

`Hello`

## Dykk dypere

Regex, eller regelmessige uttrykk, er en måte å søke etter og manipulere tekst på. I eksempelet over har vi brukt `[a-zA-Z]` for å matche alle bokstaver i det engelske alfabetet. Dette kan endres etter behov, for eksempel ved å legge til tall eller spesialtegn.

Det finnes også flere metoder for å slette tegn fra en streng, som for eksempel `erase()` og `remove()`. Men å bruke regex er ofte en mer fleksibel og effektiv måte å fjerne uønskede tegn på.

## Se også

- [C++ regex](https://www.cplusplus.com/reference/regex/)
- [C++ string manipulasjon](https://www.cplusplus.com/reference/string/)