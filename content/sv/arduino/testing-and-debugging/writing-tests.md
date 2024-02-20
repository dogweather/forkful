---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:44.023548-07:00
description: "Att skriva tester i Arduino-milj\xF6n h\xE4nvisar till processen att\
  \ skapa automatiserade tester som validerar funktionaliteten hos din kod p\xE5 Arduino-enheter.\u2026"
lastmod: 2024-02-19 22:04:57.405607
model: gpt-4-0125-preview
summary: "Att skriva tester i Arduino-milj\xF6n h\xE4nvisar till processen att skapa\
  \ automatiserade tester som validerar funktionaliteten hos din kod p\xE5 Arduino-enheter.\u2026"
title: Skriva tester
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester i Arduino-miljön hänvisar till processen att skapa automatiserade tester som validerar funktionaliteten hos din kod på Arduino-enheter. Programmerare gör detta för att säkerställa att deras kod fungerar som förväntat, reducerar buggar och förbättrar kvaliteten på deras projekt, vilket är särskilt viktigt i inbyggda system där felsökning kan vara mer utmanande.

## Hur man gör:

Arduino har inte ett inbyggt testramverk som vissa andra programmeringsmiljöer. Däremot kan du använda tredjepartsbibliotek såsom `AUnit` för enhetstestning av Arduino-kod. AUnit är inspirerad av Arduinos inbyggda bibliotek, `ArduinoUnit`, och Googles testramverk, `Google Test`.

### Exempel med AUnit:

Först, installera AUnit via Bibliotekshanteraren i Arduino IDE: gå till Sketch > Lägg till Bibliotek > Hantera Bibliotek... > sök efter AUnit och installera det.

Därefter kan du skriva tester på följande sätt:

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
Efter att ha laddat upp detta test till ditt Arduino-kort, öppna seriemonitorn för att visa testresultaten. Du bör se utdata som indikerar om varje test passerade eller misslyckades:

```
TestRunner startade på 2 test(er).
Test ledPinHigh passerade.
Test ledPinLow passerade.
TestRunner duration: 0.002 sekunder.
TestRunner sammanfattning: 2 passerade, 0 misslyckades, 0 hoppades över, 0 fick timeout, av 2 test(er).
```

Detta enkla exempel demonstrerar användningen av AUnit för att testa tillståndet på en LED-pinne. Genom att skapa tester bekräftar du att din Arduino beter sig som förväntat under olika förhållanden. Med AUnit kan du skriva mer komplexa tester, testsamlingar, och njuta av funktioner som testtimeout och uppstarts/nedstängningsprocedurer för mer avancerade scenarier.
