---
date: 2024-01-26 01:16:33.640729-07:00
description: "Hur man g\xF6r: L\xE5t oss s\xE4ga att du har en funktion p\xE5 din\
  \ Arduino som g\xF6r alldeles f\xF6r mycket, s\xE5 h\xE4r."
lastmod: '2024-03-13T22:44:38.175596-06:00'
model: gpt-4-0125-preview
summary: "L\xE5t oss s\xE4ga att du har en funktion p\xE5 din Arduino som g\xF6r alldeles\
  \ f\xF6r mycket, s\xE5 h\xE4r."
title: Refaktorisering
weight: 19
---

## Hur man gör:
Låt oss säga att du har en funktion på din Arduino som gör alldeles för mycket, så här:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // En funktion som gör för mycket
  handleEverything();
}

void handleEverything() {
  // Läs sensordata
  int sensorValue = analogRead(A0);
  // Bearbeta sensordatan
  sensorValue = map(sensorValue, 0, 1023, 0, 255);
  // Skriv ut sensordatan
  Serial.println(sensorValue);
  delay(500);
}
```

Att refaktorisera det kan se ut som att dela upp `handleEverything()` i mindre, mer fokuserade funktioner:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorValue = readSensorData();
  int processedValue = processSensorData(sensorValue);
  printData(processedValue);
  delay(500);
}

int readSensorData() {
  return analogRead(A0);
}

int processSensorData(int sensorValue) {
  return map(sensorValue, 0, 1023, 0, 255);
}

void printData(int data) {
  Serial.println(data);
}
```

Efter refaktoriseringen är `loop()` funktionen mer läsbar och varje uppgift hanteras av en dedikerad funktion, vilket gör koden lättare att hantera.

## Djupdykning
Historiskt blev refaktorisering populärt med uppgången av Agile- och testdriven utveckling (TDD) metoder, som förlitar sig på ständig kodförbättring för att anpassa sig till förändrade krav. Det finns olika verktyg och strategier för refaktorisering – som tekniken "Extrahera metod" som vi använde i vårt Arduino-exempel. Detta är väsentligt när du går från en snabb prototyp till ett stabilt projekt, där kodens läsbarhet och underhåll blir avgörande.

När du refaktoriserar är det viktigt att ha en bra uppsättning tester på plats för att säkerställa att ändringarna inte har introducerat några buggar. I Arduinovärlden är automatiserade tester inte alltid raka vägen på grund av hårdvaruberoenden, men du kan fortfarande använda enhetstester för rena logikdelar eller använda simulatorer.

Alternativ till manuell refaktorisering inkluderar användning av dedikerade refaktoriseringsverktyg, som automatiserar identifieringen av kodlukt och föreslår ändringar. Dock saknar dessa verktyg ofta nyans för mikrokontrollerkod och kanske inte finns tillgängliga i Arduinoutvecklingsmiljön.

I slutändan är refaktorisering en konst som balanserar förbättring av kodens interna struktur mot risken att introducera defekter. Det kräver att du tänker på genomförandedetaljer som minnesanvändning och processortid, speciellt på grund av mikrokontrollerarnas resursbegränsade natur.

## Se även
Du kan fördjupa dig i refaktorisering med Martin Fowlers banbrytande bok *Refaktorisering: Att förbättra designen av befintlig kod*. För en närmare titt på Arduino-specifika praxis, kolla in Arduino-utvecklingsforum och gemenskaper:

- [Arduino Forum - Programmeringsfrågor](https://forum.arduino.cc/index.php?board=4.0)
- [Refaktoriseringsguru](https://refactoring.guru/refactoring)

Kom ihåg, målet är ren, begriplig kod som framtida du, och andra, kommer att tacka dig för. Fortsätt hacka, och håll det snyggt!
