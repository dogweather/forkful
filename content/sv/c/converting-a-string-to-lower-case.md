---
title:                "Omvandla en sträng till små bokstäver"
html_title:           "C: Omvandla en sträng till små bokstäver"
simple_title:         "Omvandla en sträng till små bokstäver"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener (lower case) är när man ändrar alla stora bokstäver till små bokstäver. Detta kan vara användbart när man vill jämföra strängar utan att skilja mellan stora och små bokstäver, eller om man vill se till att all text skrivs ut enhetligt. Programmers använder vanligtvis denna funktion för att förenkla och standardisera sin kod.

## Så här:
Här är ett exempel på hur man konverterar en sträng till gemener med hjälp av den inbyggda funktionen `tolower()`:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char str[] = "Hej Världen!";
    
    for (int i = 0; str[i] != '\0'; i++) {
        printf("%c", tolower(str[i]));
    }
    
    return 0; 
}
```
Output:
`hej världen!`

## Djupdykning:
Konvertering av strängar till gemener har funnits i programmering sedan de första programmeringsspråken som C utvecklades. Tidigare var det vanligt att man använde sig av funktionen `strlwr()` för att konvertera en sträng till gemener. Men med introduktionen av unicode, är standardbibliotekets funktion `tolower()` nu den föredragna metoden för att konvertera strängar till gemener. Det finns även andra sätt att konvertera strängar till gemener, som att iterera över varje tecken och jämföra det med dess gemena version, men det är en mer tidskrävande metod och därför inte lika vanligt förekommande.

## Se även:
- [Microsoft: strlwr()](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/strlwr-wcslwr-mbstolwr-mbslwr?view=vs-2019)
- [GeeksforGeeks: Conversion of string to lower case in C/C++](https://www.geeksforgeeks.org/conversion-whole-string-uppercase-lowercase-using-stl-c/)