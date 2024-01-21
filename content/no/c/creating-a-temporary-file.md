---
title:                "Opprette en midlertidig fil"
date:                  2024-01-20T17:39:44.897112-07:00
model:                 gpt-4-1106-preview
simple_title:         "Opprette en midlertidig fil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å opprette en midlertidig fil er prosessen med å lage en fil som kun eksisterer for varigheten av et program eller en sesjon, og som ofte slettes automatisk etterpå. Programmerere gjør dette for å håndtere data midlertidig uten å påvirke permanent lagring eller for å unngå kollisjon ved samtidige prosessers tilgang.

## Hvordan:
```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    char temp_filename[] = "tempfileXXXXXX";
    int descriptor = mkstemp(temp_filename);
    
    if (descriptor == -1) {
        perror("Kan ikke opprette midlertidig fil");
        exit(EXIT_FAILURE);
    }
    
    printf("Midlertidig fil opprettet: %s\n", temp_filename);
    
    // Bruk filen for noe her ...

    // Lukk og slett midlertidig fil
    close(descriptor);
    unlink(temp_filename);
    
    return 0;
}
```
Output:
```
Midlertidig fil opprettet: tempfilec3rXWz
```

## Dypdykk:
Opprettelse av midlertidige filer i C har en historisk bakgrunn i UNIX-systemer, der filer ofte ble brukt til inter-prosess kommunikasjon og for å lagre state midlertidig. `mkstemp()` er en sikker funksjon fordi den garanterer at filnavnet er unikt, og at det ikke vil være noen filnavnkollisjon, som kan føre til sikkerhetsproblemer. Tidligere brukte noen funksjonen `tmpnam()`, men denne er utrygg fordi det er en sjanse for filnavnkollisjoner og potensielle race conditions. Det finnes alternativer som `tmpfile()`, som skaper en midlertidig fil som automatisk slettes ved programslutt, men denne gir ikke tilgang til filnavnet, som kan være nødvendig i noen situasjoner.

## Se Også:
- POSIX standarden for `mkstemp()`: https://pubs.opengroup.org/onlinepubs/9699919799/functions/mkstemp.html
- C Standardbiblioteket dokumentasjon for `tmpfile()`: https://en.cppreference.com/w/c/io/tmpfile
- Sikkerhetshensyn ved bruk av midlertidige filer: https://owasp.org/www-community/vulnerabilities/Insecure_Temporary_File