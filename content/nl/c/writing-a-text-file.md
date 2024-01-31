---
title:                "Een tekstbestand schrijven"
date:                  2024-01-28T22:12:32.658597-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand schrijven"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tekstbestand schrijven houdt in dat gegevens in een leesbaar formaat worden opgeslagen op het bestandssysteem. Programmeurs doen dit om informatie te bewaren, zoals instellingen, logs of door gebruikers gegenereerde content, voor latere ophaling en verwerking.

## Hoe:
Naar een tekstbestand schrijven in C is eenvoudig met de bibliotheekfuncties van `stdio.h`: `fopen()`, `fprintf()`, en `fclose()`. Bekijk dit eenvoudige voorbeeld:

```C
#include <stdio.h>

int main() {
    FILE *filePointer = fopen("example.txt", "w"); // Bestand openen in schrijfmodus
    if (filePointer == NULL) {
        printf("Fout bij het openen van bestand.\n");
        return 1;
    }
    
    fprintf(filePointer, "Hallo, wereld!\n"); // Naar bestand schrijven
    fprintf(filePointer, "Naar bestanden schrijven in C is eenvoudig.\n");
    
    fclose(filePointer); // Bestand sluiten
    return 0;
}
```
Voorbeelduitvoer in `example.txt`:
```
Hallo, wereld!
Naar bestanden schrijven in C is eenvoudig.
```

## Diepere duik
Sinds de oprichting met de voorouderlijke talen van C, is bestands-I/O cruciaal geweest voor programma's. Alternatieven voor `stdio.h` omvatten systeemniveau-oproepen zoals `open()`, `write()`, en `close()` van `sys/file.h`, die meer controle bieden maar complexer zijn. Bij het gebruik van `stdio.h` kan buffering prestaties beïnvloeden, dus voor grote bestanden of frequente schrijfacties, kan de functie `fflush()` nodig zijn.

## Zie ook
Voor meer over bestandsbewerkingen in C:
- Documentatie van de C-standaardbibliotheek: https://en.cppreference.com/w/c/io
- C File I/O Tutorial: http://www.cplusplus.com/doc/tutorial/files/
- Bestands-I/O beheren: https://www.gnu.org/software/libc/manual/html_node/File-System-Interface.html
