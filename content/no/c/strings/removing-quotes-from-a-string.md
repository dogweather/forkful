---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:12.665243-07:00
description: "\xC5 fjerne anf\xF8rselstegn fra en streng i C inneb\xE6rer \xE5 trekke\
  \ ut tekstinnholdet uten de innkapslende enkle (' ') eller doble (\" \") anf\xF8\
  rselstegnene. Denne\u2026"
lastmod: '2024-03-13T22:44:41.258303-06:00'
model: gpt-4-0125-preview
summary: "\xC5 fjerne anf\xF8rselstegn fra en streng i C inneb\xE6rer \xE5 trekke\
  \ ut tekstinnholdet uten de innkapslende enkle (' ') eller doble (\" \") anf\xF8\
  rselstegnene."
title: "Fjerne anf\xF8rselstegn fra en streng"
weight: 9
---

## Hvordan:
For å fjerne anførselstegn fra en streng i C, traverserer vi strengen og kopierer tegn som ikke er anførselstegn inn i en ny streng. Denne prosessen kan skreddersys for å fjerne enten bare de ledende og avsluttende anførselstegnene eller alle anførselstegn som er til stede i strengen. Nedenfor er et illustrativt eksempel som demonstrerer begge tilnærminger:

```c
#include <stdio.h>
#include <string.h>

// Funksjon for å fjerne alle anførselstegn fra en streng
void removeAllQuotes(char *source, char *dest) {
    while (*source) {
        if (*source != '"' && *source != '\'') {
            *dest++ = *source;
        }
        source++;
    }
    *dest = '\0'; // Avslutt destinasjonsstrengen med null
}

// Funksjon for å fjerne bare de ledende og avsluttende anførselstegnene fra en streng
void removeEdgeQuotes(char *source, char *dest) {
    size_t len = strlen(source);
    if (source[0] == '"' || source[0] == '\'') source++, len--;
    if (source[len-1] == '"' || source[len-1] == '\'') len--;
    strncpy(dest, source, len);
    dest[len] = '\0'; // Avslutt destinasjonsstrengen med null
}

int main() {
    char str1[] = "'Hello, World!'";
    char str2[] = "\"Programming in C\"";
    char noQuotes1[50];
    char noQuotes2[50];
    
    removeAllQuotes(str1, noQuotes1);
    printf("Alle Anførselstegn Fjernet: %s\n", noQuotes1);
    
    removeEdgeQuotes(str2, noQuotes2);
    printf("Kant Anførselstegn Fjernet: %s\n", noQuotes2);
    
    return 0;
}
```
Eksempel på utskrift:
```
Alle Anførselstegn Fjernet: Hello, World!
Kant Anførselstegn Fjernet: Programming in C
```

Disse eksemplene viser hvordan man håndterer både fjerning av alle anførselstegn til stede i strengen og målrettet fjerning av bare de ledende og avsluttende anførselstegnene.

## Dypdykk
Konseptet med å fjerne anførselstegn fra strenger har ikke betydelig historisk dybde i C, utover dens forbindelser til tidlige behov for tekstbehandling. Den enkle tilnærmingen demonstrert her er allsidig, men mangler effektivitet for veldig store strenger eller krav til høy ytelse, hvor endringer på stedet eller mer avanserte algoritmer kan være å foretrekke.

Alternativer, som å bruke `strpbrk` for å finne anførselstegn og flytte den delen av strengen som ikke er anførselstegn, kan være mer effektive, men krever en dypere forståelse av pekere og minnehåndtering i C. I tillegg har fremveksten av biblioteker med regulære uttrykk gitt et kraftig verktøysett for strengmanipulering, inkludert fjerning av anførselstegn. Men disse bibliotekene, mens kraftfulle, legger til kompleksitet og overhead som kanskje ikke er nødvendig for enklere oppgaver. Følgelig forblir den direkte tilnærmingen som vist, en verdifull ferdighet for C-programmerere, som blander enkelhet med effektiviteten for mange vanlige bruksområder.
