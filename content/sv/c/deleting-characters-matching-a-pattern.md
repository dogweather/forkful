---
title:    "C: Radera tecken som matchar ett mönster"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

I C-programmering finns det många gånger behov av att manipulera textsträngar för att uppfylla specifika krav eller för att rensa upp data. Att radera tecken som matchar ett mönster är en vanlig uppgift som kan utföras för att effektivisera och förbättra programkoden.

## Hur man gör det

En av de vanligaste funktionerna för att ta bort tecken som matchar ett mönster i C-programmering är `strtok()`. Denna funktion kan användas för att separera en textsträng vid ett specifikt tecken eller mönster. Här är ett exempel på hur man kan använda `strtok()` för att ta bort alla mellanslag i en textsträng:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Detta är en textsträng";
    char *ptr;

    /* Här kommer vår kod att ta bort mellanslagen */
    ptr = strtok(str, " ");
    while (ptr != NULL) {
        printf("%s", ptr);
        ptr = strtok(NULL, " ");
    }
    /* Utmatning: Denna är en textsträng */
    return 0;
}
```

## Fördjupning

För den som är intresserad av att lära sig mer om att ta bort tecken som matchar ett mönster i C-programmering, finns det flera andra funktioner och metoder som kan användas. Bland dessa kan nämnas `sscanf()`, `regex` och `memmove()`. Det är också viktigt att ha kunskap om teckenuppsättningar och hur man jobbar med dem i sin kod.

## Se också

- [Wikipedia: C stringhantering](https://sv.wikipedia.org/wiki/C_stringhantering)
- [Stränghantering i C](https://www.studytonight.com/c/string-handling-functions.php)
- [C-programmering för nybörjare](https://www.codingunit.com/c-tutorial-for-beginners)