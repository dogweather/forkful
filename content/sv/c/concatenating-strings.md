---
title:    "C: Sammanslagning av strängar"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför
Att konkatinera strängar är en vanlig uppgift inom programmering, särskilt inom C-programmering. Genom att kombinera två eller flera strängar kan du skapa en längre sträng som innehåller mer information. Det kan vara användbart i många olika program och funktioner.

## Hur man gör det
För att konkatinera strängar i C-programmering, behöver du först deklarera två eller flera strängar som du vill kombinera. Sedan kan du använda den inbyggda funktionen `strcat()` för att lägga till en sträng till en annan. Se nedan för ett enkelt exempel:

```C
#include <stdio.h>
#include <string.h>

int main() {
   char str1[] = "Hej";
   char str2[] = "på dig!";
   strcat(str1, str2);
   printf("Resultatet är: %s\n", str1);
   return 0;
}
```
Output:
```
Resultatet är: Hej på dig!
```

Obs: Det är viktigt att den första strängen som du skickar till `strcat()` redan har tillräckligt med utrymme för att rymma även den andra strängen.

## Djupdykning
Många kanske undrar varför man inte bara kan använda operatören `+` för att konkatinera strängar i C-programmering. En anledning är att C inte har en inbyggd strängtyp, så det är inte möjligt att använda operatorn `+` på samma sätt som i andra programmeringsspråk. Istället använder C nulltecken (`\0`) för att markera slutet på en sträng och `strcat()`-funktionen utnyttjar detta för att lägga till en sträng till en annan.

En annan anledning är att konkatinera strängar med hjälp av `strcat()` faktiskt är mer effektivt än att använda operatorn `+`. Detta beror på att `strcat()` inte skapar en ny sträng i minnet, utan ändrar bara den befintliga strängen. Detta sparar på resurser och är därför en fördelaktig metod för att kombinera strängar i C-programmering.

## Se också
* [C-programmering för nybörjare](https://www.learn-c.org/)
* [Professor Moniters programmeringsblogg](https://profmonitersblogg.com/c-programmering/)
* [Officiell C-dokumentation](https://devdocs.io/c/start)

## Se också
* [C-programmering för nybörjare](https://www.learn-c.org/)
* [Professor Moniters programmeringsblogg](https://profmonitersblogg.com/c-programmering/)
* [Officiell C-dokumentation](https://devdocs.io/c/start)