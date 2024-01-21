---
title:                "Extrahera delsträngar"
date:                  2024-01-20T17:45:02.908055-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extrahera delsträngar"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera delsträngar innebär att plocka ut specifika delar av en sträng. Programmerare gör detta för att bearbeta eller analysera specifika data, som del av en större dataset eller för att validera input.

## Så här gör du:
I C kan du använda `strncpy` för att kopiera en del av en sträng. Se till att avsluta delsträngen korrekt:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char text[] = "Hej alla glada";
    char substr[5]; // Glöm inte plats för null-tecknet!

    strncpy(substr, text + 4, 4); // Starta på index 4, ta 4 tecken.
    substr[4] = '\0'; // Lägg till null-tecken manuellt.

    printf("Delsträngen är: '%s'\n", substr); // Visar 'alla'

    return 0;
}
```
Exekveringen ovan ger output:
```
Delsträngen är: 'alla'
```

## Djupdykning
Extraktion av delsträngar i C saknar de inbyggda funktioner som finns i högnivåspråk. Historiskt har C-programmerare behövt använda ett bibliotek såsom `string.h` eller skriva egna funktioner för att hantera strängar. Det är vanligt att använda pekare och manuella iterationer för att uppnå detta. Ett alternativ till `strncpy` är att manuellt kopiera tecken för tecken. Detta kan ge mer kontroll men också öka risken för fel som buffer overflow.

## Se även
- C Standard Library documentation: https://en.cppreference.com/w/c/string/byte
- String Manipulation in C: https://www.tutorialspoint.com/cprogramming/c_strings.htm
- C Programming/String manipulation: https://en.wikibooks.org/wiki/C_Programming/String_manipulation