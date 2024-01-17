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

## Hva & Hvorfor?
Bruk av regulære uttrykk er en vanlig praksis blant programmerere for å søke og manipulere data i tekststrenger. Dette kan være spesielt nyttig når man ønsker å finne eller erstatte spesifikke mønstre i en tekst. Regulære uttrykk hjelper også med å gjøre kode mer effektiv og lesbar.

## Hvordan:
Her er et eksempel på hvordan man kan bruke regulære uttrykk i C for å finne og endre et tall i en tekststreng:

```
#include <stdio.h>
#include <string.h>
#include <regex.h>

int main(void) {
    char str[] = "Dette er en tekst med tallet 12345";
    char pattern[] = "([0-9]+)";
    regex_t regex;
    regmatch_t match[2];
    
    if (regcomp(&regex, pattern, REG_EXTENDED) != 0) {
        printf("Kan ikke kompilere uttrykket!\n");
        return 1;
    }
    
    if (regexec(&regex, str, 2, match, 0) == 0) {
        int start = match[1].rm_so;
        int end = match[1].rm_eo;
        char num[6];
        strncpy(num, &str[start], end-start);
        num[end-start] = '\0';
        printf("Fant tallet %s\n", num);
    }
    
    regfree(&regex);
    return 0;
}
```

Dette vil skrive ut "Fant tallet 12345". Her brukes `regex.h` biblioteket for å kompilere og bruke et regulært uttrykk på teksten.

## Dypdykk:
Regulære uttrykk ble introdusert av Ken Thompson i 1968 som en del av programmet QED. Det har etter det blitt en standardfunksjon i mange programmeringsspråk. I tillegg til å bruke `regex.h` i C, finnes det også alternative biblioteker som PCRE (Perl Compatible Regular Expressions) og Boost.Regex som gir mer avanserte funksjoner for å arbeide med regulære uttrykk.

Når man bruker regulære uttrykk er det viktig å vite at de er språkuavhengige, noe som betyr at koden vi viste i eksempelet over kan brukes i andre programmeringsspråk også. Det eksisterer også mange online verktøy for å teste og leke med regulære uttrykk. 

## Se også:
- [PCRE Offisiell Side](https://www.pcre.org/)
- [Boost.Regex Dokumentasjon](https://www.boost.org/doc/libs/1_75_0/libs/regex/doc/html/index.html)
- [Regular Expressions Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)