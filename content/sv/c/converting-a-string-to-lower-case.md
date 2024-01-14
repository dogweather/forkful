---
title:                "C: Omvandla en sträng till gemener"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver är en vanlig operation som ofta används i programmering. Det kan vara användbart när man behöver jämföra strängar, sortera dem eller helt enkelt göra dem mer enhetliga i utseendet.

## Hur man gör det

För att konvertera en sträng till små bokstäver i C-programmering kan man använda en inbyggd funktion som heter `tolower`. Den tar in en enskild karaktär som parameter och returnerar en motsvarande liten bokstav om den var en stor bokstav. Om karaktären redan var en liten bokstav returneras den oförändrad.

```C
#include <stdio.h>
#include <ctype.h>

int main(void) {
    char str[] = "HELLO WORLD";
    int i;

    for(i = 0; str[i]; i++) {
        str[i] = tolower(str[i]);
    }

    printf("Konverterad sträng: %s", str);

    return 0;
}
```

Output:
```
Konverterad sträng: hello world
```

Det finns också en annan funktion som kallas `strlwr`, som tar in en hel sträng som parameter och konverterar alla dess bokstäver till små. Detta är dock inte en standard C-funktion, så det beror på vilket bibliotek du använder om den är tillgänglig eller inte.

## Djupdykning

När man konverterar en sträng till små bokstäver är det viktigt att förstå att det bara fungerar för alfabetiska tecken. Om du har en sträng som innehåller icke-alfabetiska tecken, som siffror eller specialtecken, kommer de att förbli oförändrade. Detta kan leda till oönskade fel eller oväntade resultat om man inte hanterar dem korrekt.

Det finns också andra saker att tänka på när man konverterar mellan stora och små bokstäver i C. Till exempel är det viktigt att ta hänsyn till den aktuella språkinställningen för programmet, eftersom olika språk behandlar bokstäver på olika sätt. Det finns också särskilda begränsningar för vissa teckenuppsättningar, som inte kan hanteras korrekt av vissa C-funktioner.

## Se också

- [String lowercase function in C](https://www.programiz.com/c-programming/library-function/string.h/strlwr)
- [Manipulating strings in C](https://www.geeksforgeeks.org/strings-library-c/)
- [ASCII character set in C](https://www.tutorialspoint.com/ansi_c/c_ascii_character_set.htm)