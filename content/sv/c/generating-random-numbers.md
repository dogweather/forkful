---
title:                "Generera slumpmässiga nummer"
html_title:           "C: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## VarförDet finns många situationer där det är nödvändigt att generera slumpmässiga nummer, till exempel inom spelutveckling eller dataanalys. Genom att använda slumpmässiga nummer i våra program kan vi skapa variation och diversifiering, vilket kan leda till mer intressanta och realistiska resultat.  ## Hur man gör

För att generera slumpmässiga nummer i C kan du använda funktionen ```rand()```, som returnerar en slumpmässig heltalsvärde mellan 0 och ```RAND_MAX```. För att få en mindre intervall kan du använda modulus-operatören för att begränsa värdet.

Här är ett exempel på hur du kan använda ```rand()``` för att välja ett slumpmässigt heltal mellan 1 och 10:

```
#include <stdio.h>
#include <stdlib.h>

int main() {

  int num = rand() % 10 + 1;
  printf("Det slumpmässiga numret är: %d\n", num);
  
  return 0;
}
```

Detta kommer att generera en nytt heltal varje gång programmet körs.

## Djupdykning

För att öka precisionen och variationen i slumpmässiga nummer kan vi använda funktionen ```srand()```, som initierar generatorn med en startvärde. Genom att använda exempelvis tiden som startvärde kan vi få en mer slumpmässig sekvens av nummer.

Det är också viktigt att komma ihåg att ```rand()``` inte är en riktigt slumpmässig funktion utan en pseudo-random generator. Detta innebär att sekvensen av nummer som genereras är förutbestämd och kan återkomma efter en viss tid. För att undvika detta kan vi använda andra metoder, såsom att använda externa faktorer som muspekare eller tangenttryckningar som startvärde i ```srand()```.

## Se även

- Läs mer om de olika funktionerna för slumpmässiga nummer i C här: https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm
- Se hur du kan använda tiden som startvärde för att få mer slumpmässiga nummer här: https://stackoverflow.com/questions/822323/how-to-generate-a-random-number-in-c