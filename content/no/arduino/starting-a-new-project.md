---
title:                "Å starte et nytt prosjekt"
html_title:           "Arduino: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Når man starter et nytt prosjekt innen programmering betyr det at man begynner å jobbe med å lage et nytt program eller en ny funksjon. Dette kan være fordi man ønsker å løse et problem, skape noe nytt eller forbedre noe som allerede finnes. Å starte et nytt prosjekt er en vanlig del av den kreative prosessen til programmerere.

## Hvordan:
```Arduino
void setup() {
  // Sett opp koden din her
}

void loop() {
  // Legg til koden din her for å kontinuerlig kjøre programmet
}
```

```Arduino
void setup() {
  // Opprett en variabel og gi den verdien "Hei"
  String tekst = "Hei";
  // Skriv ut teksten ved hjelp av seriell kommunikasjon
  Serial.println(tekst);
}

void loop() {
  // Ingenting vil skje her siden det ikke er lagt til kode i løkken
}
```

Output: Hei

## Dypdykk:
Å starte et nytt prosjekt med Arduino er en enkel og effektiv måte å lage digitale prototyper. Dette er et verktøy som er utviklet for å gjøre det enklere for folk uten mye erfaring innen programmering å lage interaktive prosjekter. Andre alternativer for å starte et nytt prosjekt kan være å bruke andre programmeringsspråk eller å bygge en datamaskin fra bunnen av.

Arduino ble opprinnelig utviklet av en gruppe studenter fra Italia på begynnelsen av 2000-tallet. Siden da har det vokst til å bli et populært verktøy blant både hobbyister og profesjonelle utviklere. Det finnes mange ressurser og tutorials tilgjengelig online for å lære mer om Arduino og hvordan man kan bruke det til å lage spennende prosjekter.

## Se også:
- Hva er Arduino? - https://www.arduino.cc/en/Guide/Introduction
- Arduino ressurser og tutorials - https://www.arduino.cc/en/Tutorial/HomePage