---
title:                "Att göra en sträng versal"
date:                  2024-01-19
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva med versaler innebär att omvandla alla bokstäver i en sträng till stora bokstäver. Programmerare gör detta för tydlighet, för att framhäva text eller följa dataformatkrav.

## Hur man gör:
För att omvandla en sträng till versaler i C kan du använda standardbiblioteksfunktionerna. Här är ett enkelt exempel:

```c
#include <stdio.h>
#include <ctype.h>

void capitalizeString(char *str) {
    while(*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char myString[] = "hej världen!";
    capitalizeString(myString);
    printf("Capitalized: %s\n", myString);
    // Output: Capitalized: HEJ VÄRLDEN!
    return 0;
}
```

## Djupdykning
Att skriva med versaler är inget nytt inom programmering. Historiskt sett har det använts i många sammanhang, från att hantera assemblerinstruktioner till att formatera utskrifter. Det finns alternativ till `toupper` som `transform` i C++ STL om du behöver mer flexibilitet.

Implementeringsdetaljer som är viktiga att notera:


1. `toupper` fungerar per tecken.
2. Du måste casta tecknet till `unsigned char` för att undvika undefined behavior på tecken med negativa värden.
3. Glöm inte att inkludera `ctype.h` för `toupper`.
4. Var försiktig med teckenkodningar som UTF-8; standard `toupper` kanske inte fungerar som förväntat med icke-ASCII-tecken.

Det finns bibliotek som stödjer bokstavsomvandlingar för olika språk och teckenkodningar om du behöver stöd för internationella teckenuppsättningar.

## Se även
- C Standard Library documentation: https://en.cppreference.com/w/c/header/ctype.h
- Utförlig diskussion om teckenkodningar: https://www.joelonsoftware.com/2003/10/08/the-absolute-minimum-every-software-developer-absolutely-positively-must-know-about-unicode-and-character-sets-no-excuses/
- Alternativ till `toupper` med C++ `transform`: https://en.cppreference.com/w/cpp/algorithm/transform
