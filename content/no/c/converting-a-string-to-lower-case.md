---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Konverter Strengtilfelle i C: Hvordan og Hvorfor?

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver betyr å endre alle store bokstaver i en streng til deres tilsvarende små bokstaver. Dette er nyttig for å forenkle tekstsammenligning og søk, ettersom det fjerner forskjellen mellom store og små bokstaver.

## Slik gjør du:
Bruk funksjonen `tolower()` fra biblioteket 'ctype.h'. Her er et eksempel på hvordan du bruker den:

```C
#include <ctype.h>
#include <stdio.h>
#include <string.h>

void konverterTilLiten(char* s) {
    for(int i = 0; s[i]; i++){
        s[i] = tolower(s[i]);
    }
}

int main() {
    char streng[] = "HELLO, WORLD!";
    konverterTilLiten(streng);
    printf("%s\n", streng);  // output: hello, world!
    return 0;
}
```

## Dypdykk
Funksjonen `tolower()` har eksistert siden tidlig C for å lette manipulasjonen av tekst. Den konverterer en enkelt karakter til en liten bokstav hvis den er stor bokstav. En alternativ metode ville være å bruke ASCII-verdiene for å konvertere, men det kan bli rotete og lite intuitivt. Implementasjonen av `tolower()` varierer mellom biblioteker, men det er vanligvis implementert med en enkel sjekk, og deretter en konvertering ved å legge til en fast differanse mellom store og små bokstaver i ASCII-tabellen.

## Se også
- C Standard Library: https://en.cppreference.com/w/c
- 'tolower()' funksjon: https://www.cplusplus.com/reference/cctype/tolower/
- ASCII-tabel: http://www.asciitable.com/