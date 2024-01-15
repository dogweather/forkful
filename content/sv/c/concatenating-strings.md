---
title:                "Sammanslagning av strängar"
html_title:           "C: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanslå strängar är en vanlig och nyttig teknik i C-programmering. Det kan användas för att skapa dynamisk text och processa olika typer av data som är lagrade i strängar. Det kan också hjälpa till att skapa mer läsbar och effektiv kod.

## Så här gör du

För att sammanslå två strängar i C kan du använda funktionen `strcat()`. Här är ett exempel på hur du skulle kunna använda den:

````C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[20] = "Hello";
    char str2[20] = "World";

    strcat(str1, str2);
    printf("%s", str1);

    return 0;
}

````
**Output:** *HelloWorld*

I det här exemplet deklarerar vi två strängar, `str1` och `str2`, med storleken 20 tecken. Sedan använder vi `strcat()` för att sammanslå `str2` till `str1`, vilket lägger till "World" till slutet av "Hello". Slutligen skrivs den sammanslagna strängen ut.

Det är viktigt att notera att storleken på `str1` måste vara tillräckligt stor för att rymma den sammanslagna strängen. Om `str2` är längre än resterande utrymme i `str1` kommer de överflödiga tecknen att kapas eller så kan det uppstå en error. Du kan också använda `strncat()` för att säkerställa att strängen inte blir för lång.

## Deep Dive

För att förstå hur sammanslagning av strängar fungerar djupare kan vi titta på den faktiska koden bakom `strcat()` funktionen:

````C
char *strcat(char *dest, const char *src) {
    char *ptr = dest + strlen(dest);
    while (*src) {
        *ptr++ = *src++;
    }
    *ptr = '\0';
    return dest;
}
````

Funktionen `strcat()` tar in två parametrar, en destinationssträng (`dest`) och en källsträng(`src`). Den första raden i koden deklarerar en pekare `ptr` som pekar på slutet av `dest` med hjälp av `strlen()` funktionen. Sedan loopar den igenom `src` tills den når slutet av strängen och lägger till varje tecken till `dest` en efter en. Slutligen sätts ett null-tecken till slutet av `dest` för att indikera att strängen är slut.

Det finns också andra funktioner som kan användas för att sammanslå strängar, som `strncat()` eller `sprintf()`. Det är viktigt att läsa dokumentationen för dessa funktioner för att förstå deras användning och skillnader.

## Se också

- [Dokumentation för strcat()](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Exempel på sammanslagning av strängar i C](https://www.geeksforgeeks.org/strcat-strncat-functions-c/)
- [Tilldelning av strängar i C](https://www.programiz.com/c-programming/c-strings)