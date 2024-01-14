---
title:                "C: Att hitta längden av en sträng"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en grundläggande del av C-programmering. Det är viktigt att förstå hur detta fungerar eftersom det är en vanlig uppgift inom programmering. Om du vill lära dig mer om C-programmering och förbättra dina färdigheter är det viktigt att ha en god förståelse för hur stränglängd beräknas.

## Hur man gör det

För att räkna ut längden på en sträng i C använder vi funktionen `strlen()`. Den här inbyggda funktionen finns i string.h-biblioteket och kan användas genom att inkludera detta bibliotek i början av ditt program.

```C
#include <stdio.h>
#include <string.h>

int main()
{
   char str[50];
   
   printf("Skriv in en sträng: ");
   scanf("%s", str);
   
   int length = strlen(str);
   
   printf("Längden på strängen är %d tecken.", length);
   
   return 0;
}
```

### Exempel

Låt oss säga att vi vill räkna längden på strängen "Hej C-programmerare!".

```C
#include <stdio.h>
#include <string.h>

int main()
{
   char str[50] = "Hej C-programmerare!";
   
   int length = strlen(str);
   
   printf("Längden på strängen är %d tecken.", length);
   
   return 0;
}
```

Output: Längden på strängen är 19 tecken.

## Djupdykning

För att förstå hur funktionen `strlen()` fungerar måste vi först förstå hur en sträng lagras i minnet. En sträng i C är en array av tecken som avslutas med ett nolltecken (`'\0'`). Detta nolltecken används för att indikera slutet på strängen. Så när `strlen()`-funktionen körs, räknar den enbart antalet tecken i arrayen tills den når nolltecknet, vilket markerar slutet på strängen. Därför är det viktigt att ha en tillräckligt stor array för att lagra hela strängen och ett extra utrymme för nolltecknet.

## Se även

- [C-programmering för nybörjare](https://www.programiz.com/c-programming)
- [strlen() - C biblioteksreferens](https://www.programiz.com/c-programming/library-function/string.h/strlen) 
- [C-programmering: Strängar](https://www.studytonight.com/c/strings-in-c.php)