---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att ta bort tecken som matchar ett visst mönster är ett vanligt behov inom programmering. Detta används för att manipulera strängar för att tillfredsställa olika användarbehov och situationer, till exempel rensa upp indata eller normalisera text.

## Hur göra:

I C använder vi ofta biblioteksfunktionen `strpbrk()` för att uppnå detta. Låt oss ta en titt på ett exempel:

```C
#include<stdio.h>
#include<string.h>

int main() {
    char str[50] = "Hej, Världen!";
    const char chars_to_delete[10] = ",!";

    char *p = strpbrk(str, chars_to_delete);

    while (p != NULL){
        memmove(p, p+1, strlen(p));
        p = strpbrk(str, chars_to_delete);
    }
    
    printf("%s\n", str);

    return 0;
}
```

I exemplet ovan tar vi bort alla kommatecken och utropstecken från "Hej, Världen!". Utmatningen skulle bli:

```C
Hej Världen
```

## Djupdykning

Historiskt sett har teckenmanipulation som denna varit en grundläggande del av programmeringshistorien och har varit en kritisk operation sedan de tidiga dagarna av strängbehandling. 

Förutom metoden ovan finns det flera alternativa metoder för att ta bort tecken som matchar ett visst mönster i C, inklusive användning av funktioner som `strtok()` eller skrivning av anpassade funktioner baserat på tecken-vid-tecken-jämförelser.

Viktigt att notera är att, trots att det är en relativt enkel uppgift, så beror implementeringsdetaljerna ofta på specifika applikationer och krav. Noggrannhet, prestanda, och kompatibilitet kan alla vara kritiska faktorer att tänka på.

## Se även

För ytterligare information om strängmanipulation i C, se följande länkar:
- Verktyg för hantering av tecken och strängar i C: https://www.cplusplus.com/reference/cstring/
- C-programmering/strängar: https://en.wikibooks.org/wiki/C_Programming/strings
- Ta bort alla förekomster av ett givet tecken i en sträng: https://www.geeksforgeeks.org/remove-all-occurrences-of-a-character-in-a-string/