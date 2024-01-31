---
title:                "Interpolera en sträng"
date:                  2024-01-20T17:50:21.599869-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolera en sträng"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Stringinterpolering är när du blandar variabler med statisk text för att skapa en sammansatt sträng. Programmerare gör detta för att generera dynamiskt innehåll som kan ändras under programmets körning.

## Hur man gör:
```c
#include <stdio.h>

int main() {
    int age = 30;
    float height = 1.85;
    printf("Jag är %d år gammal och %fm lång.\n", age, height);
    return 0;
}
```
Utskrift: `Jag är 30 år gammal och 1.850000m lång.`

## Djupdykning
Historiskt sett har C:s `printf`-funktion varit standard för att skapa strängar med olika typer av data insprängd. Alternativ inkluderar användandet av `sprintf` för att skapa en sträng utan att direkt skriva ut den. Nyligen införda i andra språk är bokstavliga stränginterpoleringar, men C håller fast vid dess traditionella `printf`-stil. Implementeringsdetaljer kring `printf` och dess syskon beror på användandet av format specifiers (som `%d` och `%f` ovan) för att definiera hur variabeln ska formateras och visas.

## Se också:
- C Standard Library - `printf`: https://en.cppreference.com/w/c/io/fprintf
- Format Specifiers in C: https://www.cplusplus.com/reference/cstdio/printf/
- GNU C Library Manual – Formatted Output Functions: https://www.gnu.org/software/libc/manual/html_node/Formatted-Output-Functions.html
