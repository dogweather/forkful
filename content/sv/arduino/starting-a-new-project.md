---
title:                "Påbörja ett nytt projekt"
html_title:           "Arduino: Påbörja ett nytt projekt"
simple_title:         "Påbörja ett nytt projekt"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Att starta ett nytt projekt inom Arduino världen innebär att skriva kod och ansluta elektroniska komponenter för att skapa en fungerande enhet. Programmers gör detta för att realisera sina idéer och för att lära sig mer om programmering och elektronik.

# Hur gör man:

Exempel 1: Att starta ett nytt projekt med en LED-lampa.
```Arduino
void setup() {
  pinMode(LED_BUILTIN, OUTPUT);  // Definiera LED-lampan som en output för Arduino
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH);   // Sätt på LED-lampan
  delay(1000);                       // Vänta en sekund
  digitalWrite(LED_BUILTIN, LOW);    // Stäng av LED-lampan
  delay(1000);                       // Vänta en sekund
}
```
Resultat: LED-lampan blinkar med en sekunds mellanrum.

Exempel 2: Att starta ett nytt projekt med en potentiometer.
```Arduino
int potPin = A0;    // Definiera analoga pin A0 för potentiometern
int ledPin = 9;     // Definiera pin 9 för LED-lampan

void setup() {
  pinMode(ledPin, OUTPUT);    // Definiera LED-lampan som en output för Arduino
}

void loop() {
  int sensorValue = analogRead(potPin);    // Läs av potentiometerns värde
  int ledBrightness = map(sensorValue, 0, 1023, 0, 255);    // Konvertera värdet till lämplig ljusstyrka för LED-lampan
  analogWrite(ledPin, ledBrightness);    // Sätt ljusstyrkan på LED-lampan baserat på potentiometerns värde
}
```
Resultat: LED-lampan ljusstyrka justeras beroende på potentiometerns värde.

# Djupdykning:

Historisk kontext: Arduino är en öppen källkod (open-source) plattform som utvecklades år 2005 för att göra det enklare för icke-tekniska personer att börja experimentera med elektronik och programmering. Det har sedan dess blivit mycket populärt inom hobby- och utbildningsområdet.

Alternativ: Det finns många andra mikrocontrollers som liknar Arduino, såsom Raspberry Pi och Micro:bit. Men Arduino är särskilt populärt för sina enkla och användarvänliga in- och utgångar.

Implementering detaljer: Vid start av ett nytt projekt med Arduino, är det viktigt att ha en bra idé och en klar förståelse för hur olika komponenter ska anslutas och fungera tillsammans. Det är också viktigt att ha kunskap om grundläggande programmeringskoncept som loopar, villkor och variabler.

# Se också:

För mer information och inspiration, ta gärna en titt på Arduino's officiella hemsida och forum:
- [Arduino Officiell Hemsida](https://www.arduino.cc/)
- [Arduino Forum](https://forum.arduino.cc/)