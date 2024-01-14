---
title:                "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en vanligt förekommande uppgift inom Arduino-programmering. Det kan användas för spel, lottdragningar eller för att skapa variation i ett program. I denna bloggpost kommer vi att titta närmare på hur man enkelt kan generera slumpmässiga nummer med hjälp av Arduino.

## Hur man gör det

För att generera slumpmässiga nummer behöver vi använda oss av en funktion som heter ```random()```. Den här funktionen tar två argument, det lägsta och högsta värdet som det slumpmässiga numret kan vara.

Låt oss till exempel säga att vi vill generera ett slumpmässigt nummer mellan 1 och 10. Vi skulle då skriva:

```Arduino
random(1,10); // Detta kommer att ge ett slumpmässigt nummer mellan 1 och 10
```

Vi kan också kombinera ```random()``` med en variabel för att använda det genererade numret i vårt program. Till exempel:

```Arduino
int num = random(1,10); // Genererar ett slumpmässigt nummer mellan 1 och 10 och sparar det i variabeln "num"
```

Här är ett komplett exempel på hur man kan använda ```random()``` för att blanda en spelkortlek. Koden kommer att generera ett slumpmässigt kort mellan 1 och 52 och skriva ut vilket nummer som representerar vilket kort:

```Arduino
int kort = random(1,52); // Genererar ett slumpmässigt tal mellan 1 och 52 och sparar det i variabeln "kort"

if(kort <= 13){ // Om talet är mindre än eller lika med 13, så är det en spader.
  Serial.println("Spader " + String(kort));
}
else if(kort <= 26){ // Om talet är mellan 14 och 26, så är det en hjärter.
  Serial.println("Hjärter " + String(kort-13));
}
else if(kort <= 39){ // Om talet är mellan 27 och 39, så är det en klöver.
  Serial.println("Klöver " + String(kort-26));
}
else{ // Om talet är mellan 40 och 52, så är det en ruter.
  Serial.println("Ruter " + String(kort-39));
}
```

Outputen kan se ut så här:

```
Ruter 8
```

## Djupdykning

Om du vill ha ännu mer kontroll över de slumpmässiga numrena som genereras, kan du använda ```randomSeed(seed)```. Detta gör att du kan "fröa" slumpgeneratorn med en specifik början och på så sätt få samma följd av slumpmässiga nummer varje gång du kör koden. Här är ett exempel:

```Arduino
randomSeed(1234); // "Sår" slumpgeneratorn med numret 1234

// Kör sedan koden som genererar slumpmässiga nummer
```

## Se även

- [Arduino Reference - random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Arduino Project Hub - Random Number Generator using Arduino and a Potentiometer](https://create.arduino.cc/projecthub/Annydm/random-number-generator-d0332d)
- [Instructables - Random Number Generator using Arduino and a Keypad](https://www.instructables.com/Random-Number-Generator-Using-Arduino-and-Keypad/)