---
title:                "Kontrollera om en mapp finns"
html_title:           "C: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp finns kan vara ett viktigt steg i din C-programmering. Genom att utföra detta steg kan du säkerställa att din kod fungerar korrekt och undvika potentiella felmeddelanden eller kraschar. Det är även ett sätt att hantera eventuella användarfel som kan uppstå vid programkörning.

## Hur man gör det

För att kontrollera om en mapp finns i C-programmering använder man funktionen `opendir()` från `<dirent.h>` biblioteket. Detta bibliotek tillhandahåller funktioner för att hantera filsystemet och gör det möjligt att gå igenom mappar och filer.

För att använda `opendir()` behöver du först skapa en `DIR`-variabel och tilldela den värdet av mappens sökväg som du vill kontrollera. Sedan kan du använda `opendir()` för att försöka öppna mappen och kontrollera om den returnerar `NULL` eller inte. Om den returnerar `NULL` finns mappen inte, annars är mappen tillgänglig och kan användas för vidare åtgärder.

Här är ett exempel på kod som kontrollerar om en mapp finns och skriver ut ett meddelande baserat på resultatet:

```c
#include <stdio.h>
#include <dirent.h>

int main() {
    // Skapa en DIR variabel för mappen
    DIR *dir;

    // Tilldela värdet av mappens sökväg till variabeln
    dir = opendir("path/to/folder");

    // Kontrollera om mappen kunde öppnas eller inte
    if (dir == NULL) {
        // Skriv ut ett felmeddelande
        printf("Mappen finns inte!\n");
    } else {
        // Skriv ut ett bekräftelsemeddelande
        printf("Mappen är tillgänglig!\n");

        // Efter användning av mappen måste den stängas igen
        closedir(dir);
    }

    // Avsluta programmet
    return 0;
}
```

Om mappen finns kommer ovanstående kod att skriva ut "Mappen är tillgänglig!" annars skrivs "Mappen finns inte!" ut. Här kan du anpassa kodblocket för dina egna behov och inkludera fler åtgärder baserat på resultatet av `opendir()`.

## Djupdykning

För att förstå hur `opendir()` fungerar och varför den är användbar behöver vi ha en grundläggande förståelse för filsystemet i ett operativsystem. Mappar (eller kataloger) används för att organisera filer och undermappar, och dessa kan skapas, ändras och tas bort genom olika systemanrop.

När du använder `opendir()` använder du i grunden en systemanrop för att försöka öppna en mapp för läsning. Om anropet lyckas returneras en pekare till mappen, annars returneras `NULL`. Med hjälp av denna åtgärd kan du sedan utföra fler operationer på mappen, till exempel läsa filer i mappen eller skapa en ny fil i mappen.

En viktig sak att komma ihåg är att för att undvika eventuella fel eller kraschar bör du alltid stänga en mapp som du öppnar med `opendir()`, vilket kan göras med funktionen `closedir()`. Detta försäkrar att ingen oönskad åtkomst sker till mappen och att ditt program fungerar korrekt.

## Se även

- Learn C in 2021: A beginner's guide (Engelska): https://dev.to/swam/build-your-foundations-learn-c-in-2021-part-1-of-2-15cp
- C-Referens för standardbiblioteket: https://www.programiz.com/c-programming/standard-library/dirent
- Video om hanteringen av mappar i C: https://www.youtube.com/watch?v=idzPqCi0fso&ab_channel=AaronLerer