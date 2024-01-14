---
title:                "Arduino: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Random tall er en viktig del av mange Arduino-prosjekter. Det kan brukes til alt fra å lage blinkende lysmønstre til å lage tilfeldige musikalske noter. I denne bloggposten skal vi gå gjennom hvordan du kan generere random tall i dine egne prosjekter.

## Slik gjør du det

For å generere random tall på Arduino, kan du bruke funksjonen `random()` i kombinasjon med `randomSeed()` for å sikre at tallene blir mer tilfeldige.

```Arduino
// Setter randomSeed til en analog input, for eksempel A0
randomSeed(analogRead(A0));

// Genererer et tilfeldig tall fra 0 til 99
int randomTall = random(100);

// Skriver ut det tilfeldige tallet
Serial.println(randomTall);
```

Dette vil gi en tilfeldig tall mellom 0 og 99.

Du kan også bruke `random()` til å generere tilfeldige tall innenfor et gitt intervall, for eksempel mellom 10 og 50.

```Arduino
// Setter randomSeed til en analog input, for eksempel A0
randomSeed(analogRead(A0));

// Genererer et tilfeldig tall mellom 10 og 50
int randomTall = random(10, 50);

// Skriver ut det tilfeldige tallet
Serial.println(randomTall);
```

## Dypdykk

For å forstå hvordan random tall blir generert på Arduino, er det viktig å forstå at det ikke er helt tilfeldig, men heller basert på en algoritme som bruker et utgangspunkt, kalt "seed". Dette er derfor hvorfor vi bruker `randomSeed()` for å sikre at tallene blir mer tilfeldige.

Dette utgangspunktet er vanligvis basert på en analog input, som eksempelet over viser. Dette er fordi de små variasjonene i en analog input kan bidra til å gjøre tallene mer tilfeldige.

En annen viktig faktor for å generere mer tilfeldige tall er å sørge for at den aktuelle analoge inputen er koblet til noe som ikke er konstant, som for eksempel en termistor eller en lystett film.

## Se også

- [Random tall på Arduino](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Tilfeldigheter på Arduino](https://learn.adafruit.com/multi-tasking-the-arduino-part-3/intro-to-timer-interrupts)
- [Å lage et tilfeldig tall med Arduino](https://www.makerguides.com/random-number-arduino/)