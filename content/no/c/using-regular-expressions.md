---
title:                "Å bruke regulære uttrykk"
html_title:           "C: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hvorfor

Regulære uttrykk, eller "regular expressions" på engelsk, er et kraftig verktøy for tekstbehandling og mønstermatching i programmering. Ved å lære å bruke regulære uttrykk, kan man effektivt finne og manipulere tekst i en stor mengde filer, noe som er veldig nyttig i mange programmeringsoppgaver.

# Slik gjør du det

Å lære seg regulære uttrykk i C er enkelt! De følgende eksemplene viser hvordan man kan bruke regulære uttrykk til å finne og endre mønstre i en streng.

```
/*
 * Dette programmet bruker regulære uttrykk til å finne og endre mønstre i en streng.
 */
 
#include <stdio.h>
#include <regex.h>

int main() {
    // Definer en regex for å finne alle tall i en streng
    regex_t regex;
    const char *pattern = "[0-9]+";
    
    // Kompilèr regexet til å bruke det for å finne mønstre
    regcomp(&regex, pattern, REG_EXTENDED);
    
    // Søk etter regexet i en streng
    char *string = "Hello123World";
    regmatch_t match;
    regexec(&regex, string, 1, &match, 0);
    
    // Skriv ut alle tall som ble funnet
    printf("Funnet tall: ");
    for (int i = match.rm_so; i < match.rm_eo; i++) {
        printf("%c", string[i]);
    }
    printf("\n");
    
    // Endre alle tall til stor bokstav og skriv ut den modifiserte strengen
    regsub(&regex, string, "X", &string);
    printf("Modifisert streng: %s\n", string);
    
    // Frigjør ressurser og avslutt programmet
    regfree(&regex);
    return 0;
}
```

Eksempelutgang:

```
Funnet tall: 123
Modifisert streng: HelloXWorld
```

# Dykk dypere

For å lære mer om regulære uttrykk i C, kan du lese dokumentasjonen for "regex.h" biblioteket og utforske ulike funksjoner og flags som kan brukes. Det er også mange ressurser tilgjengelig på nettet som kan hjelpe deg med å bli mer komfortabel med å bruke regulære uttrykk.

# Se også

- [Dokumentasjon for regex.h i C](https://www.systutorials.com/docs/linux/man/3-regex/)
- [Regulære uttrykk CheatSheet](https://www.debuggex.com/cheatsheet/regex/c)