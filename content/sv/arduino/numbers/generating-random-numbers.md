---
title:                "Generera slumptal"
aliases: - /sv/arduino/generating-random-numbers.md
date:                  2024-01-27T20:32:47.073449-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generera slumptal"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga tal i Arduino-projekt innebär att producera värden som är oförutsägbara av design, vilket är avgörande för tillämpningar som spel, simuleringar och säkerhetssystem. Programmerare använder denna teknik för att införa variabilitet eller fatta beslut som inte bör vara deterministiska.

## Hur man gör:
Arduino tillhandahåller enkla funktioner för att generera slumpmässiga tal: `randomSeed()` och `random()`. För att börja, seeda slumpmätaregeneratorn för att säkerställa olika sekvenser av tal varje gång ditt program körs. En ofta använd metod är att seeda med en analog avläsning från en oansluten pin.

```Arduino
void setup() {
  Serial.begin(9600);
  // Initiera slumpfrö
  randomSeed(analogRead(0));
}

void loop() {
  // Generera ett slumpmässigt tal mellan 0 och 99
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // Fördröjning en sekund för läsbarhet av utdata
}
```

Programmet ovan initialiserar slumpmätaregeneratorn i `setup()`-funktionen och genererar ett nytt tal mellan 0 och 99 i varje loopiteration, och skriver ut talet till Serial Monitor.

Exempel på utdata:
```
42
17
93
...
```

## Fördjupning
Arduinos `random()`-funktion använder sig under huven av en pseudoslumpmätaregenerator (PRNG), som följer en deterministisk sekvens men ser statistiskt slumpmässig ut. Det initiala värdet, eller fröet, i sekvensen påverkar starkt dess oförutsägbarhet, därav det vanliga användandet av `randomSeed()` med något slumpmässigt inmatning som en startpunkt. Det är viktigt att notera att den slumpmässighet som genereras av Arduino är tillräcklig för de flesta hobbyprojekt men kanske inte uppfyller kriterierna för högsäkerhetstillämpningar på grund av dess förutsägbarhet över tiden. För kryptografiska ändamål är det rådligt att titta på mer sofistikerade algoritmer och hårdvarubaserade slumpmässiga nummergeneratorer (HRNGs), som kan erbjuda verklig slumpmässighet genom att utnyttja fysiska processer.
