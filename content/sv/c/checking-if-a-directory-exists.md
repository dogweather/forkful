---
title:                "C: Att kontrollera om en mapp finns"
simple_title:         "Att kontrollera om en mapp finns"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Varför: Det finns många olika skäl till varför du kanske vill kontrollera om en mapp finns. Det kan till exempel vara för att se till att dina program kan hitta och öppna filer som de behöver i en specifik mapp eller för att undvika att skriva över befintliga filer.

Hur man gör det: Det enklaste sättet att kontrollera om en mapp finns är genom att använda standardbiblioteksfunktionen `opendir()`. Här är ett kodexempel på hur du kan göra det:

```C
#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>

int main()
{
    DIR *dir = opendir("mappen_som_ska_kontrolleras");
    if (dir)
    {
        // Mappen finns!
        printf("Mappen finns!\n");
        closedir(dir);
    }
    else
    {
        // Mappen finns inte eller så går det inte att öppna den
        printf("Mappen finns inte eller så går det inte att öppna den.\n");
    }
    return 0;
}
```

Om mappen finns kommer programmet att skriva ut "Mappen finns!" och sedan stänga mappen med `closedir()` funktionen. Om mappen inte finns eller om det inte går att öppna den kommer programmet istället att skriva ut "Mappen finns inte eller så går det inte att öppna den."

Det finns också andra sätt att kontrollera om en mapp finns, till exempel genom att använda systemanropet `stat()` eller att använda `fopen()` för att försöka öppna en fil i mappen. Men `opendir()` funktionen är enkel att använda och tillräckligt för de flesta fall.

Djupdykning: Det finns ett par saker att tänka på när du använder `opendir()` för att kontrollera om en mapp finns. För det första är det nödvändigt att programmet har behörighet att läsa från den aktuella mappen för att funktionen ska fungera. Om det inte finns tillgänglig behörighet kommer funktionen att misslyckas även om mappen faktiskt finns.

För det andra är det viktigt att inse att mappen kanske inte finns även om funktionen returnerar `NULL`, särskilt om du använder relativ sökvägar istället för absoluta. Det kan bero på att mappen är borttagen eller att sökvägen inte är korrekt angiven.

Se även: För mer information om hur du kontrollerar om en mapp finns i C programmering, kan du titta på följande länkar:

- [C API Referens: opendir()](https://www.gnu.org/software/libc/manual/html_node/Opening-a-Directory.html#Opening-a-Directory)
- [How to check if a directory or file exists in C](https://www.thecrazyprogrammer.com/2016/10/how-to-check-if-a-directory-or-file-exists-in-c.html)
- [Checking if a path points to a file or directory in C](https://www.geeksforgeeks.org/checking-path-points-file-directory-c/)