---
title:                "Generering av tilfeldige tall"
html_title:           "Arduino: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Skal du programmere en robot som skal ta tilfeldige beslutninger? Eller trenger du å lage et spill hvor hvert spill prøver å være unikt? Da er generering av tilfeldige tall viktig for å skape variasjon og realisme i programmet ditt.

## Hvordan

Å generere tilfeldige tall i Arduino er enkelt og kan gjøres ved å bruke funksjonen `random()` i Arduino språket. Her er et eksempel på hvordan du kan bruke denne funksjonen til å generere et tilfeldig tall mellom 0 og 10:

```Arduino
int tilfeldigTall = random(0, 10);
Serial.println(tilfeldigTall);
```

Dette vil generere et tilfeldig tall hver gang programmet kjøres og skrive det ut i seriell monitoren. Du kan også bruke variabler i stedet for tall i `random()` funksjonen for å generere tilfeldige tall innenfor et gitt område. For eksempel:

```Arduino
int minimum = 5;
int maksimum = 15;
int tilfeldigTall = random(minimum, maksimum);
Serial.println(tilfeldigTall);
```

Dette vil generere et tilfeldig tall mellom 5 og 15.

## Dypdykk

Hvordan fungerer egentlig tilfeldige tall generering i Arduino? Bak kulissene bruker `random()` funksjonen en algoritme kalt "Pseudo Random Number Generator" (PRNG) for å generere tallene. Dette er en matematisk formel som produserer tall som virker tilfeldige, men som egentlig er beregnelige.

For å få mer tilfeldige tall kan du bruke en ekstern kilde som en fysisk analog sensor, som for eksempel en lyssensor, og bruke verdien fra denne som input til å generere tilfeldige tall. Dette vil gi en mer "ekte" tilfeldig følelse til tallene.

## Se også

Her er noen nyttige ressurser for å utforske mer om tilfeldige tall generering i Arduino:

- Offisiell dokumentasjon for `random()` funksjonen: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Mer informasjon om PRNG algoritmen: https://en.wikipedia.org/wiki/Pseudorandom_number_generator
- Eksempelprosjekt for å bruke en lyssensor til å generere tilfeldige tall: https://www.hackster.io/ziadsherif/random-number-generator-using-light-sensor-3c53b7