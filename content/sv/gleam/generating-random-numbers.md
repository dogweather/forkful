---
title:                "Gleam: Generera slumpmässiga tal"
simple_title:         "Generera slumpmässiga tal"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer kan vara en användbar funktion i olika programmeringsprojekt, oavsett om det är för att skapa spel eller för att skapa unika identifierare. Med Gleam programmeringsspråk kan du enkelt lägga till denna funktion i dina projekt.

## Hur man gör

För att generera slumpmässiga nummer i Gleam behöver du importera modulen `random` och sedan använda funktionen `float` eller `int` beroende på vilken typ av nummer du vill ha. Här är ett exempel på hur du kan använda funktionen för att generera två slumpmässiga flyttal mellan 0 och 1:

```
Gleam import random

let num1 = random.float(0.0, 1.0)
let num2 = random.float(0.0, 1.0)

IO.println(num1, num2)
```

Detta kommer att ge dig två slumpmässiga flyttal i konsolen, till exempel `0.6381780139360925, 0.82915304531743`.

Om du vill ha heltal istället kan du använda `int` funktionen och ange övre och undre gränser för de slumpmässiga numren.

```
Gleam import random

let num1 = random.int(1, 10)
let num2 = random.int(-5, 5)

IO.println(num1, num2)
```

Detta kommer att generera två slumpmässiga heltal, till exempel `7, -1`.

## Djupdykning

Bakom kulisserna använder randommodulen Gleams interna randomgenerator för att generera slumpmässiga nummer. Denna generator bygger på en algoritm som kallas "Mersenne Twister" och är en vanlig metod för att generera slumpmässiga nummer i datorprogram.

Det är också värt att notera att dessa slumpmässiga nummer egentligen inte är helt slumpmässiga utan baseras på en startpunkt, kallad "seed". Om du väljer samma "seed" kommer du att få samma sekvens av slumpmässiga nummer. Det kan vara användbart om du behöver reproducera tester eller händelser.

## Se även

- Gleam dokumentation för `random` modulen: https://gleam.run/modules/random.html
- Officiell "Mersenne Twister" algoritm dokumentation: https://en.wikipedia.org/wiki/Mersenne_Twister