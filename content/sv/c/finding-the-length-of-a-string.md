---
title:                "C: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en grundläggande operation inom programmering och är användbart för att kunna hantera textdata på ett effektivt sätt. Det är också en viktig färdighet för att kunna manipulera strängar på ett korrekt sätt och undvika buggar i dina program.

## Hur man gör

För att hitta längden på en sträng i C-programmering, kan du använda en inbyggd funktion som kallas "strlen". Den här funktionen tar en sträng som argument och returnerar längden på strängen som en heltalsvariabel. Här är ett exempel på hur du kan använda "strlen" i ditt program:

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    char str[] = "Hej hej!";
    int len = strlen(str);

    printf("Längden på strängen är: %d", len);

    return 0;
}
```

Output: Längden på strängen är: 8

Som du kan se använde vi "strlen" för att hitta längden på strängen "Hej hej!" och sedan skrev ut resultatet med hjälp av printf-funktionen.

## Djupdykning

Att förstå hur "strlen" faktiskt fungerar kan hjälpa dig att förbättra din programmeringsförmåga. Grunden för denna funktion är en loop som går igenom varje tecken i strängen tills den når slutet av strängen och räknar antalet tecken längs vägen. Detta är faktiskt en mycket effektiv lösning på problemet, eftersom det endast tar en iteration över strängen för att hitta dess längd, oavsett hur lång eller kort strängen är.

En annan viktig sak att notera är att "strlen" endast räknar tecken som är en del av själva strängen. Det spelar ingen roll om det finns osynliga tecken som null-tecken eller mellanslag på slutet av strängen, eftersom detta inte påverkar längden på själva strängen.

## Se även

- [The C Programming Language](https://www.amazon.com/Programming-Language-2nd-Brian-Kernighan/dp/0131103628) av Brian Kernighan och Dennis Ritchie
- [Fördjupning i C-strängar](https://www.programiz.com/c-programming/c-strings) på Programiz.com
- [En introduktion till C-programmering](https://www.youtube.com/watch?v=1WJf-KKLn9Y) på YouTube (på svenska)