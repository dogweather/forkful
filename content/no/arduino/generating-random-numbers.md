---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:48:29.124742-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Tilfeldige tall er numeriske verdier som ikke kan forutsees. Programmører bruker dem for å tilføre usikkerhet og variasjon i prosjekter, som spill, simuleringer og eksperimenter.

## Slik gjør du:
Generer et tilfeldig tall mellom 10 og 99:

```Arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0));
}

void loop() {
  int tilfeldigTall = random(10, 100); // 100 er ikke inkludert
  Serial.println(tilfeldigTall);
  delay(1000); // Vent i et sekund
}
```
Eksempel på utdata: 
```
34
78
45
...
```

## Dypdykk
Generering av tilfeldige tall på Arduino er ikke sann tilfeldighet; det er basert på algoritmer. `randomSeed()` gir et startpunkt for disse algoritmene, og uten den, vil sekvensen av tall være den samme hver gang. Historisk har tilfeldig tallgenerasjon vært en kompleks oppgave, men Arduino gjør det enkelt. Det finnes alternative metoder, som bruk av eksterne tilfeldige tallgeneratorer, men for mange hobbyprosjekter er `random()`-funksjonen tilstrekkelig.

## Se også:
- Arduino sin offisielle dokumentasjon på `random()` funksjonen: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Mer om tilfeldig tallgenerering: https://en.wikipedia.org/wiki/Random_number_generation
- Inndypning i `randomSeed()`: https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/