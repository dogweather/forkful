---
title:                "Arduino: Å skrive tester"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du har programmert med Arduino, har du sannsynligvis opplevd frustrasjonen av å oppdage at koden din ikke fungerer som den skal. Kanskje det er en liten feil et sted, eller du kan ikke finne ut hvorfor komponentene ikke samhandler riktig. Her er hvorfor du bør vurdere å skrive tester for Arduino-koden din: å skrive tester kan bidra til å fange potensielle feil og sikre at koden din fungerer som den skal.

## Hvordan

Å skrive tester for Arduino-koden din kan virke som en skremmende oppgave, men det trenger ikke å være det. Ved hjelp av bibliotekene "ArduinoUnit" og "ArduinoMock" kan du enkelt opprette tester for å sjekke at funksjonene dine fungerer som forventet.

For å begynne, inkluder "ArduinoUnit" biblioteket i koden din. Bruk deretter "TEST_CASE" makroen for å opprette en testenhet. Innenfor denne testenheten kan du bruke "assertEquals" funksjonen for å sammenligne forventet og faktisk output. Her er en eksempelkode:

```Arduino
#include <ArduinoUnit.h>

// Testenhet
TEST_CASE(TestFunction) {
  // Forventet output
  int expected = 5;
  // Faktisk output
  int actual = yourFunction();
  // Sammenligner forventet og faktisk output
  assertEquals(expected, actual);
}
```

Når du har skrevet alle testene dine, kan du laste dem opp til Arduino og kjøre dem ved å åpne Serial Monitor og trykke på "RUN_TESTS" knappen.

## Dypdykk

For å skrive gode tester for Arduino-koden din, er det viktig å følge noen beste praksis. For det første bør testene dine være uavhengige av hverandre og kunne kjøres i hvilken som helst rekkefølge. Dette betyr å unngå avhengighet av variabler eller funksjoner fra tidligere tester. Et annet viktig aspekt er å sikre at testene dekker alle mulige scenarier og kan fange feil på en pålitelig måte.

Det er også viktig å oppdatere testene dine når du gjør endringer i koden din. Dette sikrer at alle funksjoner fortsatt fungerer som de skal og at ny funksjonalitet ikke bryter eksisterende funksjoner.

## Se også

- "ArduinoUnit" bibliotek: https://github.com/mmurdoch/arduinounit
- "ArduinoMock" bibliotek: https://github.com/tobykurien/arduinomock

Til slutt, å skrive tester for Arduino-koden din kan spare deg for mye hodepine og gjøre det enklere å finne og rette feil i koden din. Bruk de nevnte bibliotekene og følg beste praksis for å skrive effektive og pålitelige tester. Lykke til med kodingen din!