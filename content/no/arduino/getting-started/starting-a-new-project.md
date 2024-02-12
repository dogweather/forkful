---
title:                "Å starte et nytt prosjekt"
date:                  2024-01-20T18:02:48.258209-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å starte et nytt prosjekt"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å starte et nytt prosjekt betyr å lage et nytt skript eller program fra bunnen av. Programmerere gjør dette for å skape tilpassede løsninger, lære nye ferdigheter eller utforske teknologiens grenser.

## Slik går du frem:
Her er et simpelt blinkende LED eksempel:

```Arduino
void setup() {
  pinMode(13, OUTPUT); // Setter digital pin 13 som utgang
}

void loop() {
  digitalWrite(13, HIGH);   // Skru på LED'en
  delay(1000);              // Vent i ett sekund
  digitalWrite(13, LOW);    // Skru av LED'en
  delay(1000);              // Vent i ett sekund
}
```

Når du laster opp dette, vil du se at LED'en på Arduinoen din blinker hvert sekund.

## Dykk dypere:
Arduino ble startet som et verktøy for studenter ved Interaction Design Institute Ivrea i Italia rundt 2005. Det er flust av alternativer til Arduino, som Raspberry Pi for mer komplekse prosjekter eller micro:bit for utdanning. Det som gjør Arduino unikt, er kombinasjonen av åpen kildekode-maskinvare og -programvare, som gjør det lett å dele og bygge videre på prosjekter. Ved å starte fra scratch på en Arduino, lærer du håndtering av mikrokontrollerens innganger og utganger, og programmeringsspråket C/C++.

## Se Også:
- Arduino offisielle hjemmeside: [https://www.arduino.cc/](https://www.arduino.cc/)
- Arduino nybegynnerguide: [https://www.arduino.cc/en/Guide](https://www.arduino.cc/en/Guide)
- Prosjekter på Arduino Project Hub: [https://create.arduino.cc/projecthub](https://create.arduino.cc/projecthub)
