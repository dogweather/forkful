---
title:                "Radera tecken som matchar ett mönster"
html_title:           "C: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
En vanlig uppgift som programmerare måste hantera är att ta bort tecken som matchar ett visst mönster från en sträng. Detta kan vara användbart för att rensa och omvandla data, till exempel att ta bort alla specialtecken från en användares inmatning. Ofta görs detta för att underlätta bearbetning och förbättra strängens läsbarhet.

## Så här:
För att ta bort tecken som matchar ett visst mönster i C, kan du använda funktionen `strspn()` tillsammans med `memmove()` för att skapa en ny sträng utan de matchande tecknen. Här är ett exempel som tar bort alla siffror från en sträng:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hello123World";
    char pattern[] = "0123456789";
    int length = strspn(str, pattern);
    memmove(str, &str[length], strlen(str) - length + 1);

    printf("%s", str);
    return 0;
}

//Output: HelloWorld
```

## Djupdykning:
För att förstå hur detta fungerar måste vi först förstå vad `strspn()` och `memmove()` gör. `strspn()` söker igenom en sträng och räknar hur många tecken som matchar ett visst mönster. Därefter används `memmove()` för att flytta alla tecken framåt i strängen, där längden från `strspn()` används för att hoppa över de matchande tecknen.

En annan alternativ metod skulle vara att använda `strchr()` för att leta efter de matchande tecknen och sedan sammanfoga resten av strängen utan dem.

## Se även:
[C String Functions](https://www.programiz.com/c-programming/string-handling-functions)