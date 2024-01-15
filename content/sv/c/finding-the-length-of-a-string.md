---
title:                "Hitta längden på en sträng."
html_title:           "C: Hitta längden på en sträng."
simple_title:         "Hitta längden på en sträng."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden på en sträng är en grundläggande funktion inom programmering och kan användas i många olika situationer. Genom att kunna beräkna längden på en sträng kan du till exempel säkerställa att en input från användaren inte överskrider en viss längd.

## Hur man gör

För att hitta längden på en sträng i C behöver vi använda en inbyggd funktion, `strlen()`, som finns i standardbiblioteket `string.h`. Här är en enkel kod som visar hur du kan använda `strlen()` för att hitta längden på en sträng:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hej, världen!";
    int len = strlen(str);
    printf("Strängen \"%s\" är %d tecken lång.\n", str, len);
    return 0;
}
```

**Output:**
`
Strängen "Hej, världen!" är 14 tecken lång.
`

Det första vi gör är att inkludera `string.h` för att få tillgång till `strlen()`-funktionen. Sedan deklarerar vi en sträng som vi vill hitta längden på och använder `strlen()` för att beräkna längden. Slutligen skriver vi ut längden med hjälp av `printf()`.

Det är viktigt att notera att `strlen()` returnerar en `int`, så vi måste deklarera en variabel av typen `int` för att lagra resultatet.

## Djupdykning

`strlen()`-funktionen räknar inte det sista tecknet i en sträng, vilket är nolltecknet `\0` som markerar slutet på en sträng i C. Detta betyder att om vi har en sträng med 10 tecken, kommer `strlen()` att returnera 9.

Det är också viktigt att ha i åtanke att `strlen()` bara fungerar för null-terminerade strängar, det vill säga strängar som avslutas med `\0`. Om du försöker använda den på en sträng som inte är null-terminerad kommer den inte att fungera korrekt.

## Se även

- [C: String Functions](https://www.programiz.com/c-programming/c-string-functions)
- [C Strängar](https://www.learn-c.org/en/Strings)
- [strlen() dokumentation](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)