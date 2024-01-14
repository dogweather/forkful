---
title:                "C: Att göra en sträng stor bokstav"
simple_title:         "Att göra en sträng stor bokstav"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att göra en sträng med stora bokstäver, även kallat "capitalizing a string", kan vara användbart i många olika situationer. Det kan hjälpa till att göra strängen mer läsbar eller för att matcha en specifik formatering som krävs i ett program.

## Hur man gör det

Att göra en sträng med stora bokstäver är en enkel process som kan göras med hjälp av inbyggda funktioner i C-språket. Här är ett exempel på hur man gör det:

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main()
{
    char str[20] = "hej världen";
    
    // Använda toupper() för att göra bokstäverna stora
    for(int i = 0; i < strlen(str); i++){
        str[i] = toupper(str[i]);
    }
    
    printf("%s", str); // Skriva ut "HEJ VÄRLDEN"
    
    return 0;
}
```

Det finns också andra sätt att göra en sträng med stora bokstäver, som att använda en loop och ändra ASCII-värdena eller använda en funktion som strupr(). Men att använda toupper() är det enklaste sättet i C.

## Djupdykning

För de som är intresserade av mer avancerade koncept, här är en kort förklaring om hur funktionen toupper() fungerar:

Alla tecken i ett C-program representeras av en specifierad ASCII-kod, som är en numerisk representation av tecknets position i ASCII-tabellen. Till exempel är ASCII-koden för bokstaven 'a' 97, medan bokstaven 'A' har koden 65. Funktionen toupper() använder sig av ASCII-tabellen för att omvandla små bokstäver till stora bokstäver. Den jämför ASCII-koden för varje tecken i strängen med koden för bokstäverna 'a' till 'z' och om de matchar, ökar den den med -32 för att få motsvarande stor bokstav.

## Se även

- [ASCII-table](http://www.asciitable.com/)
- [Funktionen strupr() i C](https://www.studytonight.com/c/string-manipulation-functions-in-c/strupr-function)
- [Ytterligare information om toupper()](http://www.cplusplus.com/reference/cctype/toupper/)