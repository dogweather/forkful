---
title:                "Uthenting av delstrenger"
date:                  2024-01-20T17:45:15.269478-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uthenting av delstrenger"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å trekke ut understrenger betyr at vi henter deler av en tekststreng. Programmerere gjør dette for å analysere data, validere input, eller jobbe med bestemte tekstsegmenter.

## Slik gjør du:
```C
#include <stdio.h>
#include <string.h>

int main() {
    char tekst[] = "Hallo Norge!";
    char understreng[6];

    strncpy(understreng, tekst + 6, 5);  // Henter "Norge"
    understreng[5] = '\0';               // Legger til en null-terminator

    printf("Understreng: %s\n", understreng);  // Skriver ut "Norge"
    return 0;
}
```
Output:
```
Understreng: Norge
```

## Dypdykk
I C, å trekke ut understrenger har vært en del av språket siden C's barndom. Funksjoner som `strncpy` gjør det mulig, men forsiktighet er nødvendig for å unngå buffer overflyt og sikre null-terminering. Alternativer som `strndup` kan være sikrere, men ikke alltid tilgjengelige. Detaljer som hvor du setter null-terminatoren er kritiske, da C strenger alltid slutter med `\0`.

## Se Også
- C Standard Library: https://en.cppreference.com/w/c/string/byte
- Sikker string håndtering i C: https://owasp.org/www-community/vulnerabilities/C_String_Vulnerabilities
