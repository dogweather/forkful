---
title:                "Sette streng til store bokstaver"
date:                  2024-01-19
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
"Store bokstaver": Det å gjøre om en streng til kun store bokstaver. Programmere gjør dette for å standardisere tekstfelter, forbedre lesbarheten eller for tilfeller som krever det, som akronymer.

## How to:
```C
#include <stdio.h>
#include <ctype.h>

void capitalizeString(char *str) {
    while(*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char tekst[] = "Hei, Norge!";
    capitalizeString(tekst);
    printf("%s\n", tekst); // Skriver ut: HEI, NORGE!
    return 0;
}
```

## Deep Dive
I de tidlige datasystemene var det ofte forskjell på store og små bokstaver på grunn av begrenset støtte i ASCII-tabellen. Nå har vi Unicode, som støtter et mangfold av språk og symbolske tilfeller.

Alternativer til `toupper` inkluderer manuell manipulasjon av ASCII-verdier eller bruk av funksjoner fra andre biblioteker, slik som `boost::to_upper` i C++.

Implementeringsdetaljer verdt å merke seg:
- `toupper` fra `<ctype.h>` er optimalisert for C.
- Det er lurt å passe på tegnsettet strålen din bruker; funksjoner som `toupper` håndterer som regel kun ASCII.

## See Also
- C Standard Library documentation: https://en.cppreference.com/w/c
- Unicode Standard: https://www.unicode.org/standard/standard.html
- Boost String Algorithms Library: https://www.boost.org/doc/libs/release/libs/algorithm/string/
