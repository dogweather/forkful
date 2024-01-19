---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Sök och ersätt text i programmering

## Vad & Varför?
Sök och ersätt text är en handling där en sträng identifieras (söks) och byts ut mot en annan. Programmerare gör detta för data manipulation, att korrigera fel, automatisera uppgifter, och så vidare.

## Hur man gör:
Här är ett grundläggande exempel på hur man kan söka och ersätta text i C-miljö.

```C
#include <string.h>
#include <stdio.h>

void search_replace(char* str, char* oldW, char* newW) {
    char buffer[200];
    char *pos;

    while ((pos = strstr(str, oldW)) != NULL) {
        strncpy(buffer, str, pos-str);
        buffer[pos-str] = '\0';
        strcat(buffer, newW);
        strcat(buffer, pos + strlen(oldW));
        strcpy(str, buffer);
    }

    printf("%s\n", str);
}

int main() {
    char str[] = "Hej världen!";
    char oldW[] = "världen";
    char newW[] = "Sverige";
    search_replace(str, oldW, newW);

    return 0;
}
```
Output av detta kodexempel skulle vara: "Hej Sverige!"

## Djupdykning
Sök och ersätt har länge varit ett verktyg inom textbehandling, och implementeringen av det i C var ett steg framåt för att erbjuda direkt kontroll till utvecklare. Det finns alternativa sätt att söka och ersätta text - en metod skulle vara att använda 'sed'-verktyget i UNIX.

Implementationen av denna funktion kan variera betydligt. I det ovanför angivna exemplet används inbyggda strängfunktioner som 'strstr', 'strncpy', 'strcat', och 'strcpy'. Dessa kan dock ändras beroende på den specifika sök- och ersättläsningsalgoritmen som efterfrågas.

## Se även
För mer information om sök och ersätt, se följande källor:
1. [Sök och ersätt på Stack Overflow](https://stackoverflow.com/questions/779875/how-do-i-replace-a-string-in-place-in-c)
2. [C Tutorial: strängmanipulation](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
3. [GNU Sed](https://www.gnu.org/software/sed/manual/sed.html)