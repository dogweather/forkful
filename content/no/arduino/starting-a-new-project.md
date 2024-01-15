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

## Hvorfor

Å starte et nytt prosjekt kan være både spennende og utfordrende. Ved å bruke Arduino, en åpen kildekode elektronikk plattform, kan du utforske og skape dine egne interaktive innretninger og roboter. Det er en flott måte å lære om elektronikk og programmering, samtidig som du har det gøy.

## Slik gjør du det

Du kan programmere en Arduino ved å følge disse enkle trinnene:

```Arduino
// Sett "LED" til å bli styrt av pin 13
int LED = 13;

void setup() {
// Sett pin 13 som utgang
  pinMode(LED, OUTPUT);
}

void loop() {
// Skru på LED'en i 1 sekund
  digitalWrite(LED, HIGH);
  delay(1000);
// Skru av LED'en i 1 sekund
  digitalWrite(LED, LOW);
  delay(1000);
}
```

Dette eksempelet vil få en LED-lys til å blinke på og av hvert sekund. Ved å endre tiden i `delay` funksjonen, kan du endre hvor lenge LED'en skal være på eller av.

## Gå dypere

Når du starter et nytt prosjekt med Arduino, er det viktig å ha en god idé og en tydelig plan for hvordan du ønsker å implementere den. Tenk på komponenter du trenger, som sensorer eller motorer, og hvordan du skal koble dem til Arduino. Det er også nyttige ressurser som Arduino dokumentasjon og online samfunn som kan hjelpe deg når du står fast.

## Se også

- [Offisiell Arduino nettside](https://www.arduino.cc/)
- [Arduino dokumentasjon](https://www.arduino.cc/reference/en/)
- [Arduino forum](https://forum.arduino.cc/)