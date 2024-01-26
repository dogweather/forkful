---
title:                "Konvertera en sträng till gemener"
date:                  2024-01-20T17:37:51.425174-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertera en sträng till gemener"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
Att konvertera en sträng till gemener innebär att omvandla alla stora bokstäver i strängen till små. Programmerare gör detta för datasäkerhet, för att standardisera data för lagring eller sökfunktioner, och för användargränssnitt som inte är känsliga för skiftläge.

## How to: (Hur gör man:)
I C kan du använda standardbiblioteksfunktionen `tolower()` från `<ctype.h>` för att konvertera enskilda tecken till gemener. För att konvertera en hel sträng skapar du en loop som går igenom strängen tecken för tecken.

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while(*str) {
        *str = tolower((unsigned char) *str);
        str++;
    }
}

int main() {
    char myString[] = "Hej Sverige!";
    toLowerCase(myString);
    printf("Gemener: %s\n", myString);
    return 0;
}
```

Sample output:
```
Gemener: hej sverige!
```

## Deep Dive (Djupdykning)
För länge sedan var det ingen självklarhet att datorer skulle hantera stora och små bokstäver på samma sätt som idag. ASCII-standarden, som definierade en gemensam kodning för tecken, införde först konceptet med att varje bokstav har en stor och en liten form. Tidigt var varierande och inkonsekvent.

Det finns alternativ till `tolower()`, som att manuellt subtrahera värdet som skiljer stora och små bokstäver i ASCII (Vanligtvis 32 för engelska bokstäver). En annan är att använda funktioner som `strlwr()`, men den är inte standard och finns inte i alla C-implementeringar.

Konverteringsfunktionen `tolower()` hanterar oftast bara standard ASCII-tecken. För att hantera internationella tecken och teckensnitt som går utanför standard ASCII behövs utökad funktionalitet som tar hänsyn till lokala inställningar eller Unicode-tecken.

## See Also (Se Även)
- C Standard Library Reference: https://en.cppreference.com/w/c
- ASCII Table and Description: https://www.asciitable.com/
- Unicode Consortium: https://home.unicode.org/
