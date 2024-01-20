---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hitta längden på en sträng innebär att räkna antalet tecken som finns i den. Programmerare gör detta för att hantera data effektivt när strängoperationer behövs.

## Så här:

Att hitta längden på en sträng i C på dess mest grundläggande nivå kan utföras med hjälp av `strlen` funktionen från `string.h` biblioteket.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char minStrang[] = "Hej världen!";
    int length = strlen(minStrang);
    printf("Strängens längd är %d tecken.", length);
    return 0;
}
```

Output:

```
Strängens längd är 12 tecken.
```

## Djupgående Analys

1. Historisk kontext: `strlen` funktionen har varit en del av standard C-biblioteket sedan början. C själv introducerades på 1970-talet och från början var strängen ett array av tecken.
   
2. Alternativ: Förutom `strlen`, kan du även använda en slängkod för att beräkna stränglängd. Men det ökar kodens komplexitet, så det är mer lämpligt att använda inbyggda funktioner när det är möjligt.

```C 
int strLength(char* str) {
   int length = 0;
   while(*str != '\0') {
       length++;
       str++;
   }
   return length;
}
```

3. Implementeringsdetaljer: `strlen` function går igenom varje tecken i strängen och räknar antalet tills den hittar ett noll tecken (`\0`) som markerar slutet på strängen.

## Se även 

1. Officiell dokumentation av C standardbibliotek: [https://www.cplusplus.com/reference/cstring/](https://www.cplusplus.com/reference/cstring/)
2. Ytterligare detaljer på Wikipedia: [https://en.wikipedia.org/wiki/C_string_handling](https://en.wikipedia.org/wiki/C_string_handling)