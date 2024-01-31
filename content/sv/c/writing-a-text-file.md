---
title:                "Skriva en textfil"
date:                  2024-01-19
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva en textfil innebär att lagra data i en fil på disken i textformat. Programmerare gör detta för att spara inställningar, resultat eller utbyta data med andra program.

## Steg för Steg
Skriva en grundläggande textfil i C:

```C
#include <stdio.h>

int main() {
    FILE *fil = fopen("exempel.txt", "w"); // Öppna en fil för skrivning
    if (fil == NULL) {
        perror("Fel vid öppning av filen");
        return 1;
    }

    fprintf(fil, "Hej! Det här är en textfil.\n"); // Skriv text till filen
    fprintf(fil, "C-programmering är kul.\n");

    fclose(fil); // Stäng filen
    return 0;
}
```

Exempel på utdata för 'exempel.txt':

```
Hej! Det här är en textfil.
C-programmering är kul.
```

## Fördjupning
Textfilsbehandling i C har sina rötter från dess föregångare och har varit en del av standardbiblioteket sedan språkets födelse. Alternativ till `FILE *` och `fprintf` inkluderar användning av lågnivåsystemsanrop som `open` och `write` i POSIX-system. När man implementerar filskrivning är felhantering viktigt; glöm inte att kontrollera `NULL`-returer och använd `errno` för detaljerad diagnos.

## Läs Mer
- C Standard Library: https://en.cppreference.com/w/c/io
- Learn C Programming: https://www.learn-c.org/
- C File I/O Guide: https://www.geeksforgeeks.org/basics-file-handling-c/
