---
title:                "Skapa en temporär fil"
date:                  2024-01-20T17:39:42.386488-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skapa en temporär fil"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skapa en temporär fil innebär att du skapar en fil som är tillfällig och används under programmets körning. Programmerare gör detta för att hantera data som inte behöver sparas långsiktigt eller för att undvika skräp i filsystemet.

## Hur gör man:
```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    char tempFileName[] = "my_temp_XXXXXX";
    int fileDescriptor = mkstemp(tempFileName);
    
    if (fileDescriptor == -1) {
        perror("Kan inte skapa tempfil");
        exit(EXIT_FAILURE);
    }
    
    printf("Temporär fil skapad: %s\n", tempFileName);
    
    // Använd filen här...
    
    // Stäng och radera filen
    close(fileDescriptor);
    remove(tempFileName);
    
    return 0;
}
```
**Exempel på output:**
Temporär fil skapad: my_temp_abcdef

## Fördjupning
Skapandet av temporära filer har varit en del av programmering sedan tidiga datasystem. `tmpfile()` och `mkstemp()` är två funktioner som används i C. `tmpfile()` skapar en anonym temporär fil som automatiskt raderas när programmet avslutas. `mkstemp()` kräver dock att du hanterar raderingen själv men ger mer kontroll med en unik filnamn. `mkstemp()` ger också ökad säkerhet jämfört med `tmpnam()`, som kan skapa säkerhetsrisker genom förutsägbara filnamn.

## Se även
- C Standard Library documentation: https://en.cppreference.com/w/c/io/tmpfile
- Man-page for mkstemp(3): https://linux.die.net/man/3/mkstemp
- POSIX Files and Directories: https://pubs.opengroup.org/onlinepubs/9699919799/functions/mkstemp.html
