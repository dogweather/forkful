---
title:                "Att göra en sträng versal"
html_title:           "C: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att göra om en textsträng till versaler (eller 'stora bokstäver') innebär att varje bokstav i strängen byts ut till dess motsvarighet i versaler. Programmerare gör det ofta för att standardisera textdata för jämförelser eller för att förbättra läsbarheten.

## Så här gör du:

Här är ett enkelt exempel på hur du kan använda C-programmeringsspråket för att omvandla en sträng till versaler.

```C
#include <ctype.h>
#include <stdio.h>

void skriv_ut_i_versaler(char str[]) {
    for(int i = 0; str[i]; i++){
        putchar(toupper(str[i]));
    }
    printf("\n");
}

int main(){
    char mening[] = "hej, världen!";
    printf("Ursprungliga meningen: %s\n", mening);
    printf("Meningen i versaler: ");
    skriv_ut_i_versaler(mening);
    return 0;
}
```
När du kör koden ovan ska den producera följande resultat:

```
Ursprungliga meningen: hej, världen!
Meningen i versaler: HEJ, VÄRLDEN!
```
## Djupdykning

Att omvandla en sträng till versaler har en lång historia i programmering, speciellt i applikationer där textjämförelser är nödvändiga och där textdata inte alltid anges med samma bokstavsstorlek.

När det gäller alternativ till `toupper` funktionen, finns det ingen inbyggd funktion för att omvandla hela strängar till versaler i C. Men man kan byta ut `toupper` med en egen funktion om behovet uppstår.

Funktionen för att omvandla en sträng till versaler i detalj: först går vi igenom varje tecken i strängen med en `for`-loop. Sedan använder vi `toupper`-funktionen som omvandlar det nuvarande tecknet till versaler om det är en liten bokstav, och gör inget om det redan är en stor bokstav eller något annat tecken (som kommatecken, punkter, etc). Slutligen skriver vi ut den omvandlade strängen med `putchar`.

## Se också

- [toupper() funktion i C på tutorialspoint.com](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)
- [Teckensträngar i C på programiz.com](https://www.programiz.com/c-programming/c-strings)