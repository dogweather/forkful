---
title:                "Sammenslåing av strenger"
date:                  2024-01-20T17:34:20.118814-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammenslåing av strenger"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Sammenslåing av strenger er det å lime sammen to eller flere tekststykker til ett. Vi gjør dette for å bygge setninger, lage dynamiske meldinger eller behandle variabelt tekstinnhold.

## How to:
```C
#include <stdio.h>
#include <string.h>

int main() {
    char hello[] = "Hei, ";
    char world[] = "verden!";
    char combined[50]; // Stor nok til begge strenger pluss null-terminator

    strcpy(combined, hello);     // Kopierer 'hello' til 'combined'
    strcat(combined, world);     // Legger til 'world' etter 'hello' i 'combined'

    printf("%s\n", combined);    // Skriver ut: Hei, verden!
    return 0;
}
```

## Deep Dive
I gamle dager, før funksjoner som `strcpy` og `strcat` eksisterte, måtte programmerere ofte kopiere strenger manuelt. Historisk sett kunne dette lede til mange feil. 

I dag finnes det flere måter å sammenslå strenger på i C. Foruten standardbibliotekets `strcpy` og `strcat`, finnes det `strncat` som begrenser antall tegn og beskytter mot buffer overflyt. For C11 og nyere, `strcat_s` og `strcpy_s` er alternativer som legger ekstra vekt på sikkerhet.

Når det gjelder implementasjon, er det viktig å huske på minnekapasiteten. Pass på at målstrengen har nok plass for den sammenslåtte strengen, inkludert null-terminatoren.

## See Also
- C Standard Library Documentation: https://en.cppreference.com/w/c/string/byte
- Secure C Programming Guide: https://wiki.sei.cmu.edu/confluence/display/c/SEI+CERT+C+Coding+Standard
- Understanding Pointers in C: https://www.learn-c.org/en/Pointers
