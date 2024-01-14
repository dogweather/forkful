---
title:    "C: Kontrollera om en katalog finns"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför

Att kunna kontrollera om en mapp existerar kan vara en bra färdighet att ha när du utvecklar program i C. Det kan hjälpa dig att undvika felmeddelanden och hantera olika scenarier i din kod.

## Hur man gör

För att kontrollera om en mapp existerar kan du använda funktionen `opendir()`. Den här funktionen tar in en sökväg som argument och försöker öppna mappen. Om mappen finns kommer funktionen att returnera en pekare till den öppnade mappen, annars returnerar den `NULL`.

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    char* path = "/home/user/Documents"; // vägen till din mapp
    DIR* dir = opendir(path); // försöker öppna mappen
    if (dir) {
        printf("Mappen finns!\n"); // skriver ut om mappen existerar
        closedir(dir); // stänger mappen
    } else {
        printf("Mappen finns inte!\n"); // skriver ut om mappen inte existerar
    }
    return 0;
}
```

När du kör programmet kommer du att se antingen "Mappen finns!" eller "Mappen finns inte!" beroende på om mappen existerar eller inte.

## Djupdykning

När du använder `opendir()` funktionen måste du vara noga med att läsa innehållet av den returnerade pekaren och stänga mappen när du är klar med den. Om du glömmer att stänga mappen kan det leda till läckande minne i ditt program.

Det finns också andra sätt att kontrollera om en mapp existerar, som att använda kommandot `stat()` eller systemanropet `access()`. Det är viktigt att välja rätt metod beroende på vad du vill uppnå och vilken plattform du utvecklar för.

## Se också

- [C opendir() dokumentation](https://www.cplusplus.com/reference/cstdio/opendir/)
- [How to Check if a Directory Exists in C](https://www.tutorialspoint.com/how-to-check-if-a-directory-exists-in-c-language)
- [Linux System Programming in C: Directories](https://www.computernetworkingnotes.com/linux-tutorials/linux-system-programming-in-c-directories.html)