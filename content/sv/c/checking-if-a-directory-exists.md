---
title:                "Kontrollera om en mapp existerar"
html_title:           "C: Kontrollera om en mapp existerar"
simple_title:         "Kontrollera om en mapp existerar"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När man programmerar, kan man ibland behöva kontrollera om en mapp finns eller inte. Det är en process som innebär att man verifierar om en specificerad mapp existerar på datorn eller inte. Detta kan vara användbart i många olika scenarier, till exempel när man vill hantera filer och organisera dem i en hierarkisk struktur.

## Hur gör man?
Det finns flera sätt att kontrollera om en mapp existerar i C. Ett sätt är att använda funktionen `opendir()` från standardbiblioteket `<dirent.h>`. Här är ett exempel på hur man skulle kunna implementera detta i sin kod:

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    // Ange sökvägen till mappen som ska kontrolleras
    char* path = "C:/Users/John/Documents/example_folder/";

    // Öppna mappen med en anpassad pointer
    DIR* dir = opendir(path);

    // Kolla om mappen existerar
    if (dir) {
        printf("Mappen finns!\n");
        closedir(dir);  // Stäng mappen
    }
    else {
        printf("Mappen finns inte.\n");
    }

    return 0;
}
```

Om mappen existerar, kommer programmet att skriva ut "Mappen finns!". Om mappen inte existerar, kommer det istället att skriva ut "Mappen finns inte.".

## Djupdykning
Funktionen `opendir()` är en del av standardbiblioteket `<dirent.h>` som används för att hantera fildirektiv. Den öppnar och läser en mapp, vilket gör det möjligt att kontrollera om den existerar eller inte.

En alternativ metod för att kontrollera om en mapp existerar är att använda funktionen `stat()` från standardbiblioteket `<sys/stat.h>`. Denna funktion returnerar information om en fil eller mapp, inklusive om det är en mapp eller inte. Detta kan då användas för att verifiera om en mapp existerar eller inte.

För att implementera `stat()` i koden ovan, skulle man behöva lägga till denna kod före `if`-satsen:

```C
struct stat s;
// Kontrollera om s är en mapp
if (stat(path, &s) == 0 && S_ISDIR(s.st_mode)) {
    printf("Mappen finns!\n");
}
```

Genom att lägga till `S_ISDIR(s.st_mode)` i `if`-satsen kan man bekräfta att det är en mapp som `path` pekar på.

## Se även
- [Dokumentation för direktivhantering i C](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/dirent-dot-h)
- [Dokumentation för `stat()` i C](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/stat-functions)