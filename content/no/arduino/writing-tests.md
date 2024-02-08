---
title:                "Skrive tester"
aliases:
- no/arduino/writing-tests.md
date:                  2024-02-03T19:29:54.903424-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skrive tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive tester i Arduino-miljøet refererer til prosessen med å lage automatiserte tester som validerer funksjonaliteten til koden din på Arduino-enheter. Programmerere gjør dette for å forsikre seg om at koden deres fungerer som forventet, reduserer feil og forbedrer kvaliteten på prosjektene deres, spesielt viktig i innebygde systemer hvor feilsøking kan være mer utfordrende.

## Hvordan:

Arduino har ikke et innebygd testrammeverk som noen andre programmeringsmiljøer. Imidlertid kan du bruke tredjepartsbiblioteker som `AUnit` for enhetstesting av Arduino-kode. AUnit er inspirert av Arduinos innebygde bibliotek, `ArduinoUnit`, og Googles testframework, `Google Test`.

### Eksempel med AUnit:

Først, installer AUnit via Bibliotekbehandleren i Arduino IDE: gå til Skisse > Inkluder bibliotek > Behandle biblioteker... > søk etter AUnit og installer det.

Deretter kan du skrive tester slik:

```cpp
#include <AUnit.h>

test(ledPinHigh) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, HIGH);
  assertTrue(digitalRead(ledPin));
}

test(ledPinLow) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, LOW);
  assertFalse(digitalRead(ledPin));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
  // Tom
}
```
Etter å ha lastet opp denne testen til Arduino-kortet ditt, åpne Seriell Monitor for å se testresultatene. Du bør se en utskrift som indikerer om hver test ble bestått eller feilet:

```
TestRunner startet med 2 test(er).
Test ledPinHigh bestått.
Test ledPinLow bestått.
TestRunner varighet: 0.002 sekunder.
TestRunner sammendrag: 2 bestått, 0 feilet, 0 hoppet over, 0 tidsavbrutt, av totalt 2 test(er).
```

Dette enkle eksemplet demonstrerer bruk av AUnit for å teste tilstanden til en LED-pin. Ved å lage tester, bekrefter du at din Arduino oppfører seg som forventet under forskjellige forhold. Med AUnit, kan du skrive mer komplekse tester, testpakker og nyte funksjoner som test tidsavbrudd og oppsett/nedrivningsprosedyrer for mer avanserte scenarioer.
