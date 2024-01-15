---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Generering av slumpmässiga tal är en viktig funktion inom Arduino-programmering eftersom det kan bidra till att skapa mer dynamiska och varierade kretsar och projekt.

## Så här gör du

För att generera slumpmässiga tal i Arduino kan du använda dig av funktionen `random(min, max)` där `min` är det minsta möjliga talet och `max` är det största möjliga talet. Här nedan följer ett exempel på hur du kan använda denna funktion för att generera fem slumpmässiga tal mellan 1 och 10:

```Arduino
int slumpmässigtTal;

for (int i = 0; i < 5; i++) {
  slumpmässigtTal = random(1, 10);
  Serial.println(slumpmässigtTal);
}
```

Detta kodexempel kommer att skriva ut fem slumpmässiga tal i seriell monitor och varje gång du kör koden kommer dessa tal att vara olika.

Du kan även använda dig av funktionen `randomSeed()` för att starta en sekvens för slumpmässiga tal baserat på ett specifikt startnummer. På så sätt kan du skapa en mera förutsägbar slumpmässig sekvens som du kan använda i din kod.

## Djupdykning

Vad som händer bakom kulisserna när du genererar slumpmässiga tal i Arduino är att en algoritm kallas för Linear Congruential Generator (LCG) används. Denna algoritm är enkel och effektiv men har vissa begränsningar i hur slumpmässiga talen kan vara. Om du vill ha en mera avancerad algoritm kan du använda dig av en extern enhet, såsom ett random access memory (RAM), som kan ge mera slumpmässiga resultat.

## Se även

- [Officiell Arduino dokumentation om Random](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Mer information om Linear Congruential Generator](https://en.wikipedia.org/wiki/Linear_congruential_generator)