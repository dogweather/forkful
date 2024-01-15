---
title:                "Omvandla en sträng till gemener"
html_title:           "C: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Det är vanligt att behöva omvandla en sträng till små bokstäver i olika programmeringsprojekt. Det kan till exempel vara för att göra jämförelser mellan strängar mer tillförlitliga eller för att anpassa användarinputs. Genom att förstå hur man konverterar en sträng till små bokstäver i C kan du öka din kunskap och effektivitet som programmerare.

## Så här gör du

För att konvertera en sträng till små bokstäver i C används funktionen `tolower()` från standardbiblioteket `ctype.h`. Här är ett exempel på hur det kan användas:

```C
#include <stdio.h>
#include <ctype.h>

int main() {

    // Definiera en sträng med blandade versaler och gemener
    char str[] = "HeLlo WorLD";

    // Loopa igenom varje tecken i strängen
    for (int i = 0; i < strlen(str); i++) {
        
        // Använd tolower() för att konvertera tecknet till små bokstäver
        str[i] = tolower(str[i]);
    }

    // Skriv ut den konverterade strängen
    printf("%s\n", str); // hello world

    return 0;
}
```

I det här exemplet används en `for`-loop för att gå igenom varje tecken i strängen och `tolower()` används för att konvertera tecknet till små bokstäver. Den konverterade strängen skrivs sedan ut med hjälp av `printf()`-funktionen.

## Djupdykning

För att förstå hur `tolower()`-funktionen fungerar är det viktigt att ha grundläggande kunskap om teckenkodning. I ASCII-kodningen har varje bokstav en unik numerisk representation som används av datorn för att lagra och behandla text. De stora bokstäverna har lägre numeriska värden än de små bokstäverna. Till exempel har den stora bokstaven "A" det numeriska värdet 65 och den lilla bokstaven "a" har värdet 97.

När `tolower()` anropas, kontrollerar den först om tecknet är en stor bokstav genom att använda funktionen `isupper()` från `ctype.h`. Om det är fallet omvandlas tecknet till sin motsvarande lilla bokstav genom att addera värdet 32, vilket är skillnaden mellan de stora och små bokstävernas numeriska värden i ASCII-kodningen.

## Se även

- [ASCII](https://sv.wikipedia.org/wiki/ASCII)
- [ctype.h](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)