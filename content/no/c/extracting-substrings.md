---
title:    "C: Utvinne understrønger"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor?

Mange ganger når man jobber med strenger i et C-program, kan det hende at man ønsker å isolere en del av teksten for å få tilgang til eller manipulere den separat. Dette er når å hente ut substrings kan være nyttig. Substrings er en del av en større tekststreng som vi ønsker å hente ut og jobbe med uavhengig av resten av strengen. Dette gjør det mulig å lage mer dynamiske og komplekse programmer som kan behandle tekst på en mer presis måte.

## Hvordan?

For å hente ut substrings fra en streng i C, kan vi bruke funksjonen `strncpy` som er tilgjengelig i standardbiblioteket `string.h`. Denne funksjonen kopierer en bestemt mengde tegn fra en kildestreng til en målstreng. La oss se på et eksempel:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char original_string[] = "Hei, min venn!";
    char substring[10];
    
    // Kopier de første 3 tegnene fra original_string og lagre det i substring
    strncpy(substring, original_string, 3);
    
    // Legg til avslutningstegnet for å unngå uønsket data
    substring[3] = '\0';
    
    // Skriv ut resultatet
    printf("Substring: %s\n", substring);
    
    return 0;
}
```

Dette programmet vil skrive ut "Hei" siden vi har valgt å kopiere de første 3 tegnene fra `original_string` og lagret det i `substring`. Det er viktig å merke seg at `strncpy` kopierer et bestemt antall tegn, så vi må også inkludere et avslutningstegn for å unngå uønsket data.

## Større forståelse

For å få en dypere forståelse av hvordan substrings fungerer i C, er det viktig å forstå hvordan strenger er lagret i minnet. I C er en streng en sekvens av tegn som avsluttes med et nulltegn. Når vi henter ut en substring, bruker vi egentlig bare en peker til en del av den opprinnelige strengen. Dette betyr at vi ikke har to separate strenger, men heller to pekere som peker til forskjellige deler av den samme strengen.

Det er også viktig å merke seg at substrings ikke alltid trenger å være like lange som den opprinnelige strengen. Med hjelp av funksjoner som `strchr`, `strpbrk`, `strspn` og `strstr` i `string.h` kan vi også hente ut substrings basert på spesifikke kriterier, som for eksempel å finne en viss karakter eller et bestemt ord i en tekststreng.

## Se også

- [C-programmering: Strenger](https://www.systutorials.com/docs/linux/man/3-stpcpy/)
- [C-programmering: Arrays](https://www.tutorialspoint.com/cprogramming/c_arrays.htm)
- [Standardbiblioteket `string.h` i C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)