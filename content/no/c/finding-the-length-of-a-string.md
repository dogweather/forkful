---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å finne lengden på en streng handler om å telle antall karakterer i en tekststreng. Programmerere gjør dette for å manipulere, validere eller sammenligne strenger effektivt.

## Hvordan Du Gjør Det:
I C programmering, bruker vi ofte `strlen()` funksjonen for å finne lengden på en streng. Her er et enkelt eksempel:

```C
#include <stdio.h>
#include <string.h>
  
int main() {
    char string[] = "Programmering";
    printf("Lengden av strengen er: %lu", strlen(string));
    return 0;
}
```

Når det kjøres, vil det utspytte: 
```
Lengden av strengen er: 13
```

## Dypdykk:
C standardbiblioteket har gitt `strlen()` funksjonen siden det gamle C89 standard. Funksjonen teller antall karakterer i en C-streng før den treffer den avsluttende null byte (`\0`).
Alternativer til `strlen()` vil være å iterere gjennom strengen manuelt med en løkke, selv om dette kan være mindre effektivt og mer feilutsatt.
Hoveddetaljen å merke seg er at `strlen()` ikke teller den avsluttende null-byte når den returnerer lengden, så alltid ta dette til betraktning i din kode.

## Se Også:
1. [C Library - `strlen()`](http://www.cplusplus.com/reference/cstring/strlen/)
2. [C String Handling](https://en.wikipedia.org/wiki/C_string_handling)
3. [C Programming/String manipulation](https://en.wikibooks.org/wiki/C_Programming/String_manipulation)