---
title:    "C: Sökning och byte av text"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en viktig funktion inom programmering för att enkelt kunna ändra delar av texten i vår kod. Det är också ett effektivt sätt att göra massor av ändringar på en gång istället för att manuellt gå igenom varje enskild rad.

## Hur man gör

För att söka och ersätta text i C-programmering, använder vi oss av en inbyggd funktion som heter `str_replace()`. Denna funktion tar emot tre parametrar:

- En pekare till den sträng som vi vill utföra sökningen och ersättningen på
- En sträng som vi vill söka efter
- En sträng som vi vill ersätta söksträngen med

Här är ett exempel på hur vi kan använda denna funktion för att söka och ersätta text i en sträng:

```C
#include <stdio.h>
#include <string.h>

int main() {
   char str[] = "Hej alla programmerare! Välkommen till min blogg.";

   // Söker efter "alla" och ersätter med "ni"
   str_replace(str, "alla", "ni");

   printf("%s", str);
   
   return 0;
}

// Output: Hej ni programmerare! Välkommen till min blogg.
```

Som ni kan se har strängen "alla" blivit ersatt med "ni" i den ursprungliga strängen. Detta visar hur kraftfullt det kan vara att kunna göra massor av ändringar med bara en funktion.

## Djupdykning

När vi kallar på `str_replace()`-funktionen, så sker sökningen och ersättningen endast på en kopia av den ursprungliga strängen. Detta betyder att den ursprungliga strängen förblir oförändrad. Om vi vill att ändringarna ska göras permanent på den ursprungliga strängen, måste vi istället tilldela resultatet av funktionen till den ursprungliga strängen.

Funktionen `str_replace()` är även fallkänslig, vilket innebär att den tar hänsyn till versaler och gemener vid sökningen och ersättningen. Om vi vill att sökningen och ersättningen ska vara oberoende av storlek på bokstäver, kan vi använda oss av en inbyggd funktion som heter `strcasse()` för att konvertera bägge strängarna till samma fall innan vi anropar `str_replace()`.

Nu när ni har förstått grunderna för att söka och ersätta text i C-programmering, så finns det en hel del mer avancerade tekniker och funktioner som ni kan utforska för att bli ännu effektivare i era textmanipuleringsuppgifter.

## Se även

- [C-stringar](https://www.w3schools.in/c-tutorial/c-strings/)
- [C Standardbiblioteket](http://www.cplusplus.com/reference/cstring/)
- [Officiell C-dokumentation](https://www.open-std.org/jtc1/sc22/wg14/)