---
title:                "Arduino: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Ibland kan du behöva ta bort vissa tecken från en sträng i ditt Arduino-program. Det kan vara av olika skäl, till exempel för att göra databehandling mer effektiv eller för att hantera användarens inmatning på ett mer flexibelt sätt. I den här blogginlägget kommer vi att gå igenom hur man kan ta bort tecken som matchar ett visst mönster från en sträng i Arduino-programmering.

## Så här gör du

Först måste vi inkludera standardbiblioteket `string` i vårt program. Sedan kan vi använda funktionen `indexOf()` för att hitta positionen för det första tecknet som matchar det mönster vi vill ta bort. Sedan använder vi funktionen `substring()` för att skapa en ny sträng som innehåller allt före det matchande tecknet och allt efter det. Här är en kodexempel:

```Arduino
#include <string>

String str = "Detta är en teststräng";
int pos = str.indexOf("test"); //hittar positionen för första förekomsten av "test"
String newStr = str.substring(0, pos) + str.substring(pos + 4); //skapar en ny sträng utan "test"
```

I det här exemplet använder vi `substring()` två gånger för att skapa en ny sträng utan det matchande ordet "test". Vi måste också lägga till en offset på 4 i den andra `substring()` eftersom det är längden på ordet "test". Annars kommer vi att få ett extra mellanslag i vår nya sträng.

En annan lösning är att använda funktionen `replace()` som finns i standardbiblioteket `string`. Denna funktion tar två parametrar, först det sökta mönstret och sedan det tecken (eller sträng) som ska ersätta det. Här är ett annat kodexempel:

```Arduino
#include <string>

String str = "Detta är en teststräng";
String newStr = str.replace("test", ""); //skapar en ny sträng utan "test"
```

Som du kan se, är det mycket enklare att använda `replace()` eftersom den tar hand om att hitta positionen och ändra strängen åt dig.

## Djupdykning

Om du vill ta bort flera förekomster av ett mönster från en sträng kan du använda en `while`-loop tills `indexOf()` returnerar -1, vilket betyder att det inte finns fler förekomster av mönstret i strängen. 
Här är ett exempel där vi tar bort alla förekomster av bokstaven "a" från en sträng:

```Arduino
#include <string>

String str = "Detta är en teststräng";
int pos = str.indexOf("a");
while (pos >= 0) { //så länge det finns en förekomst av "a" i strängen
    str = str.substring(0, pos) + str.substring(pos + 1); //ta bort tecknet på positionen
    pos = str.indexOf("a"); //hitta nästa förekomst av "a"
}
```

Nu när vi har gått igenom hur man tar bort tecken som matchar ett visst mönster i Arduino, kan du experimentera med olika mönster och se vilken metod som passar bäst för dina behov.

## Se också

Om du vill lära dig mer om strängmanipulering i Arduino, kan du läsa följande artiklar från vår officiella dokumentation:

- [String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [indexOf()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/)
- [substring()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [replace()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)

Tack för att du läste och lycka till med dina Arduino-projekt!