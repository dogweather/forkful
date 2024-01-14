---
title:                "Arduino: Borttagning av tecken som matchar ett mönster"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
Att ta bort tecken som matchar ett mönster är en användbar teknik för att rensa upp och hantera textsträngar i ditt Arduino-program. Det kan hjälpa dig att effektivisera din kod och se till att den endast innehåller relevanta tecken.

## Hur man gör
Det finns flera sätt att ta bort tecken som matchar ett visst mönster i din textsträng. Här är några exempel:

```Arduino
String str = "Abc123Def456Ghi";
str.remove('1'); // Resultat: Abc23Def456Ghi
str.remove('2', '3'); // Resultat: AbcDef456Ghi
str.remove('a', 'f'); // Resultat: bc123Def456Ghi
```

I det första exemplet använder vi funktionen "remove()" för att ta bort tecknet '1' från textsträngen "Abc123Def456Ghi". I det andra exemplet tar vi bort alla tecken mellan '2' och '3', och i det tredje exemplet tar vi bort alla tecken mellan 'a' och 'f'. Du kan också använda "substring()" funktionen för att ta bort en viss del av textsträngen.

## Djupdykning
Att ta bort tecken som matchar ett mönster är en effektiv teknik när du vill filtrera bort oönskad information från en textsträng. Det är också användbart när du behöver omforma en sträng till en annan, t.ex. när du jobbar med formattering eller sökningar.

Det är viktigt att vara noga med vilka tecken du tar bort, eftersom det kan påverka den slutgiltiga textsträngen på ett oväntat sätt. Se till att testa din kod väl för att undvika misstag.

## Se också
- [Arduino Reference - Strings](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino String remove() function](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/remove/)
- [Arduino String substring() function](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)