---
title:                "Slette tegn som matcher et mønster"
html_title:           "C: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Hvorfor

Det er ofte nødvendig å søke gjennom og behandle tekststrenger i et program. En vanlig oppgave er å slette bestemte tegn som samsvarer med et visst mønster. Dette kan for eksempel være uønskede spesialtegn eller mellomrom som trenger å fjernes før videre behandling av strengen. Å lære å slette tegn basert på et mønster i C kan være en nyttig kunnskap for å håndtere tekstbehandling i programmering.

# Hvordan

Vi kan bruke funksjonen `strpbrk()` i C for å søke etter et mønster i en streng og slette alle tegn som samsvarer med dette mønsteret. Syntaxen for denne funksjonen er som følger:

```C
char* strpbrk(const char* str, const char* accept);
```

Her er `str` strengen som skal søkes gjennom, mens `accept` er et sett av slettbare tegn som skal matches. Funksjonen vil returnere en peker til den første samsvarende tegnet, eller `NULL` hvis mønsteret ikke finnes i strengen.

La oss se et eksempel på hvordan vi kan bruke `strpbrk()` funksjonen for å slette mellomrom og punktum fra en streng:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Dette er en streng.";
    char accept[] = " .";
    char* ptr = strpbrk(str, accept); // Finner første samsvarnde tegn
    while (ptr != NULL) {
        *ptr = '_'; // Erstatter med underscore-tegn
        ptr = strpbrk(ptr + 1, accept); // Søker etter neste match
    }
    printf("%s", str);
    return 0;
}
```

Dette vil generere følgende output:

```
Dette_er_en_streng_
```

# Deep Dive

En nærmere titt på `strpbrk()` funksjonen avdekker at den egentlig er en variant av `strchr()` funksjonen som søker etter ett tegn i en streng. `strpbrk()` søker derimot etter et sett av tegn og er derfor ofte mer praktisk når man skal slette flere tegn.

En annen viktig funksjon for å slette tegn er `strchr()` funksjonen som returnerer en peker til det første forekomsten av et gitt tegn i en streng. Syntaxen for denne funksjonen er som følger:

```C
char* strchr(const char* str, int c);
```

Her er `str` strengen som skal søkes gjennom, mens `c` er tegnet som skal matches. Hvis tegnet ikke finnes i strengen, vil funksjonen returnere `NULL`.

# Se også

- [strpbrk() dokumentasjon på programmerings.net](https://www.programmerings.net/c/function/strpbrk)
- [strchr() dokumentasjon på microsoft.com](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/strchr-strchrn-strchrnp-wmbsinc-memchr-memchrw-memchrw)
- [Intro til strings i C for nybegynnere (på norsk)](https://www.itstudios.no/lang/no/Strings-Sekvenser.php)