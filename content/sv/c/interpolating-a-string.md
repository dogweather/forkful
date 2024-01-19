---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Stränginterpolering i C

## Vad och Varför?
Stränginterpolering är en teknik för att infoga variabler direkt i en sträng. Programmerare gör detta för att effektivt formatera och manipulera strängdata.

## Hur man gör:
I C, använd `printf` funktionen för stränginterpolering. Format specifier `%s` representerar string.

```C
#include<stdio.h>

int main() {
   char name[30] = "Kalle";
   printf("Hej, %s! Hur mår du?\n", name);
   return 0;
}
```

Utskriften blir:

```
Hej, Kalle! Hur mår du?
```

## Djupdykning
1. Historisk kontext: C språk har stöd för stränginterpolering sedan dess tidigaste versioner. 
2. Alternativ: Du kan också använda `sprintf` för att interpolera en sträng och lagra den i en buffert.
3. Implementeringsdetaljer: `%s` är en format specifier som säger till `printf` funktionen att en sträng kommer att infogas.

```C
char name[30] = "Kalle";
char greeting[50];

sprintf(greeting, "Hej, %s! Hur mår du?", name);
printf("%s\n", greeting);
```

## Se Även
För en djupare förståelse av stränginterpolering och formatering, kolla på följande källor:

1. [C Tutorial på formaterade utdata](https://www.learn-c.org/en/Formatted_output)
2. [printf och scanf i C](https://www.geeksforgeeks.org/printf-scanf-c-language/)
3. [String Interpolation i C](https://www.dummies.com/programming/c/string-interpolation-in-c/)