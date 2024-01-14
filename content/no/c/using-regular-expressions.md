---
title:                "C: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hvorfor

Å bruke regulære uttrykk (regex) i programmering kan virke som en skremmende oppgave, men det er faktisk en svært nyttig verktøy for å søke og manipulere tekststrenger. Med regex kan du enkelt finne og erstatte spesifikke mønstre i en tekst, noe som sparer tid og gjør koden din mer effektiv.

# Hvordan

For å bruke regex i C programmeringsspråket, må du inkludere biblioteket "regex.h" i koden din. Deretter kan du bruke funksjoner som `regcomp()` og `regexec()` for å definere og søke etter et regex-mønster i en tekststreng.

```C
#include <stdio.h>
#include <regex.h>

int main(){
    regex_t regex;
    char *pattern = "([0-9]+)";
    char *text = "Det er 2020, og snart 2021.";
    int result = regcomp(&regex, pattern, REG_EXTENDED);

    if(result == 0){ // Kontrollerer om kompileringen var vellykket
        regmatch_t match;
        while(regexec(&regex, text, 1, &match, 0) == 0){ // Hvis det er flere treff i teksten, vil while-loop fortsette å søke
            printf("Treff på posisjon %d\n", match.rm_so); // Skriver ut begynnelsen på treffet (rm_so)
            printf("Treff: %.*s\n", match.rm_eo - match.rm_so, text + match.rm_so); // Skriver ut lengden på treffet (rm_eo - rm_so) og selve treffet ved hjelp av substring
            text += match.rm_eo; // Endrer startposisjonen for teksten som søkes i, slik at det ikke blir flere treff på samme mønster
        }
    }

    return 0;
}
```

Output:
```
Treff på posisjon 7
Treff: 2020
Treff på posisjon 14
Treff: 2021
```

# Dypdykk

Regex i C kan virke komplisert med sine egne spesifikke syntaksregler, men det er vel verdt å lære. For å definere et regex-mønster, bruker du spesifikke tegn og uttrykk som representerer forskjellige deler av en tekst. For eksempel, `.` står for et vilkårlig tegn, `[0-9]` representerer tall fra 0 til 9, og `+` betyr at et uttrykk må forekomme minst én gang eller flere ganger. Ved å kombinere disse og andre tegn, kan du lage komplekse mønstre som gir mer nøyaktige treff i teksten din.

Regex er også en viktig del av sanitizing av inndata i programmering. Ved å bruke regex kan du filtrere ut uønsket inndata som for eksempel spesialtegn eller ugyldige tegn, noe som kan være avgjørende for sikkerheten i et program.

# Se Også

- [Regex Tutorial på w3schools](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [Online Regex Testverktøy for å eksperimentere med regex-mønstre](https://regexr.com/)
- [C Dokumentasjon for regex.h biblioteket](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)