---
title:                "C: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Det finns många anledningar till varför någon skulle behöva söka och ersätta text i sitt C-program. Det kan vara för att korrigera stavfel eller ändra variablename för att göra koden mer läsbar. Oavsett anledningen, är det ett vanligt verktyg för programmerare att ha i sin verktygslåda.

## Hur man gör

Söka och ersätta text i C-program kan göras med hjälp av olika funktioner. Letar man efter en enkel textsträng att ersätta, kan man använda sig av standardfunktionen "strreplace". Om man behöver ersätta flera textsträngar eller mer komplicerade mönster, kan regex-funktionen "regcomp" vara mer lämplig.

Här är ett exempel på hur man skulle kunna använda "strreplace":

```
#include <stdio.h>
#include <string.h>

int main(){
    char str[] = "Hej världen";
    char *new_str = strreplace(str, "Hej", "Tjena");
    printf("%s", new_str);
    return 0;
}```

Denna kod skulle ersätta "Hej" med "Tjena" i strängen "Hej världen" och skriva ut "Tjena världen".

## Djupdykning

Att söka och ersätta text kan verka enkelt, men det kan vara bra att ha en grundläggande förståelse för hur dessa funktioner fungerar bakom kulisserna. Funktionen "strreplace" byter ut en given textsträng med en annan, medan "regcomp" använder regex-mönster för att söka efter och ersätta mönster i en textsträng.

Det finns också olika sätt att implementera sök- och ersättningsalgoritmer, som påverkar hastigheten och effektiviteten hos funktionerna. Det kan vara intressant att läsa mer om dessa olika metoder och hur de påverkar ditt program.

## Se även

- ["How to Use the strreplace Function in C" (engelska)](https://www.techwalla.com/articles/how-to-use-the-strreplace-function-in-c)
- ["Regular Expressions in C" (engelska)](https://www.regular-expressions.info/c.html)
- ["Searching and Replacing in C" (engelska)](https://www.codingame.com/playgrounds/15439/searching-and-replacing-in-c)