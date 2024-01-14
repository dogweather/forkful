---
title:    "C: Att hitta längden på en sträng"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna hitta längden på en sträng är en grundläggande färdighet inom programmering. Det är viktigt att förstå hur man gör detta eftersom det är en viktig del av att hantera textinmatning och hantering av data.

## Hur man gör det
För att hitta längden på en sträng i C-programmering kan du använda funktionen `strlen()`. Detta kan göras genom att inkludera biblioteket `<string.h>` i ditt program och sedan använda funktionen i ditt kodblock genom att skriva:
```C
#include <string.h>

int main() {
    char str[] = "Hej! Det här är en sträng."; // Definiera en sträng som ska mätas
    int length = strlen(str); // Använd strlen() för att hitta längden på strängen
    printf("Längden på strängen är %d.", length); // Skriv ut längden på strängen
    return 0;
}
```

Detta kommer att skriva ut "Längden på strängen är 26." eftersom det är antalet tecken i den givna strängen.

## Djupdykning
En sträng i C-programmering är egentligen bara en array av tecken, med sista tecknet som är noll. Detta nolltecken kallas också för "nullterminering" och indikerar för datorn att detta är slutet på strängen. När `strlen()` funktionen kör, söker den igenom strängen tills den når detta nolltecken och räknar antalet tecken som kommer före det.

Det är också värt att notera att när du mäter längden på en sträng, exkluderas inte nolltecknet från längden. Så i vårt exempel ovan kommer längden att vara 25, men när man skriver ut längden adderar vi 1 eftersom vi också vill inkludera nolltecknet som en del av strängen.

## Se även
- [C-programmering på W3Schools (på svenska)](https://www.w3schools.com/cpp/cpp_strings.asp)
- [Mer information om `strlen()` funktionen från C Reference (på engelska)](https://www.codingunit.com/c-tutorial-the-functions-strcpy-and-strlen)