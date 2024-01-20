---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/extracting-substrings.md"
---

{{< edit_this_page >}}

# Artikel: Extrahera Understrängar i C Programmering

## Vad & varför?
Extraktion av understängar i C innebär att skapa nya strängar genom att ta ut en del från ursprungliga strängar. Detta används för att enkelt och effektivt manipulera och analysera data.

## Hur man gör:
Här är hur man kodar för att extrahera understängar i C:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[20] = "Hej Sverige";
    char substr[10];

    strncpy(substr, str, 5);
    substr[5] = '\0';

    printf("%s\n", substr);

    return 0;
}
```

Utskriften av koden ovan skulle vara:

```
Hej S
```

## Djupdykning
Historisk sett har extraktion av understängar i C hjälpt till att förfina och förbättra datainsamling och analys. Det finns flera sätt att göra detta på: vissa använder `strncpy`, andra kanske använder pekare.

När det gäller att extrahera understängar, är det viktigt att notera att C språket inte direkt stöder strängtypen. Istället, representeras strängar som en array av tecken, som slutar med en NULL-tecken (`\0`), vilket markerar slutet på strängen. 

## Se även
För mer information och relaterade ämnen, behandlade på djupet, besök följande länkar:

1. [C programming Guide on Programiz](https://www.programiz.com/c-programming/c-strings)
2. [C String handling functions on GeeksforGeeks](https://www.geeksforgeeks.org/string-handling-in-c-set-1/)
3. [C Programming Strings on W3Schools](https://www.w3schools.com/C/c_strings.asp)