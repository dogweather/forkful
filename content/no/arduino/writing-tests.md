---
title:                "Skriving av tester"
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-tests.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor bry seg med å skrive tester for Arduino-programmering? Vel, for å spare tid og unngå feil! Ved å skrive tester kan du identifisere og rette feil tidlig i utviklingsprosessen, noe som kan spare deg for mye frustrasjon og tid senere.

# Hvordan gjøre det

For å skrive tester i Arduino, kan du bruke biblioteket "Arduino Unit". Her er et eksempel på hvordan du kan teste en funksjon som legger sammen to tall og returnerer resultatet:

```Arduino
#include <ArduinoUnit.h>

int addNumbers(int a, int b) {
  return a + b;
}

unittest(with_two_numbers) {
  int result = addNumbers(2, 3);
  assertEqual(result, 5);
}

unittest(with_negative_numbers) {
  int result = addNumbers(-5, -10);
  assertEqual(result, -15);
}
```
Som du kan se, bruker vi "unittest" for å definere og gi navn til testen vår. Inne i testen kaller vi på funksjonen vår og bruker "assertEqual" for å sjekke om resultatet er det vi forventer. Hvis en test feiler, vil du få en feilmelding som hjelper deg med å finne og rette feilen.

# Dypdykk

Å skrive tester for Arduino kan virke som en unødvendig ekstra jobb, men det kan faktisk gjøre utviklingsprosessen mer effektiv og pålitelig. Ved å ha tester som dekker alle deler av koden din, kan du sikre at ikke bare hovedfunksjonaliteten fungerer som den skal, men også de mindre detaljene.

Det finnes også andre gode biblioteker for testing i Arduino, som "FastLED" og "Unity". Det viktigste er å finne et bibliotek som passer dine behov og forstår hvordan du kan dra nytte av det.

# Se også

Her er noen nyttige ressurser for å lære mer om testing i Arduino og hvordan du kan implementere det i din egen kode:

- [Arduino's offisielle guide for testdriven utvikling](https://www.arduino.cc/en/Guide/Testdriven)
- [Slik skriver du bedre tester for Arduino med FastLED](https://medium.com/@meffy/skriver-gode-tester-for-arduino-med-fastled-a273c60715eb)
- [Arduino's forum for testing og debugging](https://forum.arduino.cc/index.php?board=30.0)

Lykke til med testing av dine Arduino-prosjekter!