---
title:                "C: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

I många programmeringsprojekt är det viktigt att ha korrekt formaterad data, och ofta kan det innebära att konvertera en sträng till gemener eller versaler. Det kan vara en viktig del av att skapa en användarvänlig gränssnitt eller att kontrollera indata för att undvika fel.

## Hur man gör

Det finns flera sätt att konvertera en sträng till gemener i C, men det vanligaste sättet är att använda den inbyggda funktionen `tolower()`. Den tar en karaktär som argument och returnerar en gemener om den ursprungliga karaktären var en versal, annars returnerar den samma karaktär.

```C
#include <stdio.h>
#include <ctype.h>

int main()
{
    char str[] = "Hej, VÄRLDEN!";
    int i = 0;

    while(str[i])
    {
        str[i] = tolower(str[i]);
        i++;
    }

    printf("%s", str);

    return 0;
}
```

Koden ovan använder en while-loop för att loopa igenom varje karaktär i strängen och konvertera den till gemener med hjälp av `tolower()`-funktionen. Slutresultatet blir "hej, världen!".

## Djupdykning

Det är viktigt att notera att `tolower()`-funktionen endast fungerar för versaler och inte hanterar åäö korrekt om man använder standard ASCII-teckenuppsättning. För att få korrekt konvertering av åäö-nuri kallas C standardbiblioteket `wchar.h` och funktionen `towlower_l()`.

En annan viktig aspekt att tänka på är att konverteringen inte sker på plats i minnet, utan istället skapas en ny sträng med de konverterade karaktärerna. Detta kan innebära att extra minne måste allokeras, vilket kan vara problematiskt i vissa fall.

## Se även

- [C Standardbiblioteket](https://www.ibm.com/docs/sv/is compilers/rhapsody/8.5.3?topic=Standard-library)
- [wchar.h Dokumentation](https://www.cplusplus.com/reference/cwchar/)
- [toupper() Funktionen i C](https://www.geeksforgeeks.org/toupper-function-in-c/)