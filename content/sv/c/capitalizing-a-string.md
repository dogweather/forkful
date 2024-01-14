---
title:                "C: Att Skriva Ut En Sträng Med Stor Bokstav"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna ändra storleken på bokstäver i en sträng är en viktig färdighet för alla som programmerar i C. Det kan hjälpa dig att göra din kod mer läsbar och välstrukturerad, samt möjliggöra funktioner som att jämföra strängar.

## Så här gör du

Att ändra storleken på bokstäver i en sträng i C är en relativt enkel uppgift. Det första du behöver göra är att inkludera standardbiblioteket `string.h` i din kod. Detta ger dig tillgång till funktioner som kan manipulera strängar.

För att ändra storleken på bokstäver i en sträng kan du använda antingen `toupper()` eller `tolower()` funktionen. Dessa funktioner tar in en enskild karaktär som parameter och returnerar den motsvarande versalen eller gemena bokstaven. Om karaktären redan är en stor bokstav respektive liten bokstav, returneras den utan några ändringar.

Här är ett exempel på hur du skulle kunna använda dessa funktioner:

```
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hej, världen!";
    int i;

    for (i = 0; i < strlen(str); i++) {
        str[i] = toupper(str[i]);
    }

    printf("%s", str);

    return 0;
}
```

**Output:** HEJ, VÄRLDEN!

I det här exemplet skapar vi en sträng med texten "Hej, världen!" och använder sedan en loop för att ändra storleken på varje bokstav till versaler med hjälp av `toupper()` funktionen. Därefter skrivs den ändrade strängen ut till konsolen.

## Djupdykning

Förutom `toupper()` och `tolower()` funktionerna finns det andra sätt att ändra storleken på bokstäver i en sträng i C. Till exempel kan du använda `sprintf()` funktionen för att ändra storleken på varje karaktär i en sträng till versaler eller gemener. Detta kan vara användbart om du behöver utföra flera olika typer av manipulationer på en sträng.

Det är också viktigt att notera att storleksändringar kan variera beroende på vilket teckensnitt och vilket skriftspråk som används. Till exempel kan en karaktär ha en annan versal och gemener i det engelska alfabetet jämfört med det svenska alfabetet. Så se till att ha detta i åtanke när du arbetar med strängar i C.

## Se även

- <https://www.programiz.com/c-programming/library-function/string.h/toupper>
- <https://www.geeksforgeeks.org/toupper-function-in-c/>
- <https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm>