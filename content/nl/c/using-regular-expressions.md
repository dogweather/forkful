---
title:                "Reguliere expressies gebruiken"
date:                  2024-01-28T22:09:14.407378-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguliere expressies gebruiken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Reguliere expressies (regex) zoeken, matchen en manipuleren strings. Programmeurs gebruiken ze voor tekstvalidatie, zoeken en transformaties, wat de verwerking van tekst versnelt.

## Hoe:
C heeft geen ingebouwde regex-ondersteuning, maar je kunt bibliotheken zoals `regex.h` gebruiken. Hier is een eenvoudige patroonmatch.

```c
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int resultaat;
    char *patroon = "^hallo";
    char *tekst = "hallo wereld";

    // Compileer regex
    resultaat = regcomp(&regex, patroon, REG_EXTENDED);

    if (resultaat) {
        printf("Regex compilatie mislukt.\n");
        return 1;
    }

    // Voer regex uit
    resultaat = regexec(&regex, tekst, 0, NULL, 0);
    
    // Controleer op match
    if (!resultaat) {
        printf("Match gevonden.\n");
    } else if (resultaat == REG_NOMATCH) {
        printf("Geen match.\n");
    } else {
        printf("Regex uitvoering mislukt.\n");
    }

    // Ruim regex op
    regfree(&regex);

    return 0;
}
```
Voorbeelduitvoer:
```
Match gevonden.
```

## Diepgaand
Reguliere expressies worden sinds de jaren '50 gebruikt en zijn wijdverspreid geraakt met Unix's `ed` en `grep`. Alternatieven in C omvatten stringfunctiebibliotheken en aangepaste parsers, maar regex is veelzijdiger. Achter de schermen implementeert `regex.h` regex-functionaliteit, meestal via NFA (Niet-deterministische Eindige Automaat) of DFA (Deterministische Eindige Automaat) engines.

## Zie ook
- POSIX standaard: https://pubs.opengroup.org/onlinepubs/9699919799/
- Reguliere Expressies (regex) tutorial: https://www.regular-expressions.info/
- POSIX regex in C: http://man7.org/linux/man-pages/man3/regcomp.3.html
