---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Strängsammanfogning är processen för att kombinera två eller flera strängar i ett enkelt uttryck. Utvecklare gör det för att förbereda meddelanden, skapa SQL-frågor, eller helt enkelt för att bearbeta textdata på olika sätt.

## Hur gör man:

Följande kodbit demonstrerar användningen av `strcat()` och `strncat()` för strängsammansättning i C:

```C
#include <stdio.h>
#include <string.h>

int main() {
   char str1[50] = "Hej ";
   char str2[] = "Sverige!";
   
   strcat(str1, str2);
   
   printf("%s\n", str1);
   
   char str3[50] = "Hej ";
   char str4[] = "världen!";
   
   strncat(str3, str4, 7);
   
   printf("%s\n", str3);
   
   return 0;
}
```

Output:

```shell
Hej Sverige!
Hej världen!
```

## Djup Dykning:

Strängsammanfogning har använts i C sedan dess början. Tidigare versioner av C hade ingen inbyggd funktion för att utföra denna, så utvecklare var tvungna att skriva sina egna. Lösningen var att introducera `strcat()`. En aspekt att notera är att `strcat()` lägger till det andra strängargumentet till slutet av det första. 

Alternativt finns det `strncat()`, som är en säkrare version eftersom den låter programmeraren specifiera ett maximalt antal tecken som ska kopieras från källan till målet. Denna funktion förhindrar överlöpningar av strängbuffertar. Du kan också skapa din egen skräddarsydda sammansättningsfunktion i C om du vill.

Implementationen av `strcat()` och `strncat()` är tillräckligt komplicerat att man oftast är bättre att lita på standardbibliotekets rutiner. `strcat()` använder en enda pekare för att scanna två strängar. Först skanner den genom den första strängen till '\0', sedan skriver över '\0' med tecken från den andra strängen, och lägger till sin egen '\0'.

## Se även:

- `strcat` på Cplusplus: [http://www.cplusplus.com/reference/cstring/strcat/](http://www.cplusplus.com/reference/cstring/strcat/)
- `strncat` på Cplusplus: [http://www.cplusplus.com/reference/cstring/strncat/](http://www.cplusplus.com/reference/cstring/strncat/)