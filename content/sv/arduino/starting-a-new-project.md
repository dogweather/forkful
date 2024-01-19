---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att påbörja ett nytt projekt är processen att skapa en helt ny kod från början. Programmerare gör det för att lösa specifika problem, utveckla färdigheter eller för att utforska ypperlig teknik.

## Hur:

Låt oss skriva en enkel Arduino-kod som blinkar en LED:

```Arduino
int ledPin = 13;

void setup() {                
  pinMode(ledPin, OUTPUT);     
}

void loop() {
  digitalWrite(ledPin, HIGH);  
  delay(1000);                 
  digitalWrite(ledPin, LOW);   
  delay(1000);                 
}
```

Vid exekvering, blinkar LED-lampan på och av varje sekund.

## Fördjupning

Historiskt sett, Arduino är värt att nämna eftersom det är en öppen källkodsplattform som har gjort programmering tillgänglig för alla, oavsett teknisk bakgrund. Alternativt kan man använda andra mikrokontrollerplattformar som Raspberry Pi eller Beaglebone men Arduino är känd för sin enkelhet. Projektets detaljer är beroende på specifika krav samt komplexiteten.

## Se också:

För mer komplexa projekt, titta på dessa tutorials:
- [Arduino Programming with NET and Sketch](https://www.arduino.cc/en/Main/Software)
- [Arduino Programming Guide](https://www.codecademy.com/learn/learn-arduino)

För att dyka djupare in i programmering, besök dessa forum:
- [The Arduino Forum](https://forum.arduino.cc)
- [Arduino Stack Exchange](https://arduino.stackexchange.com) 

Lycka till med ditt Arduino-programmeringsäventyr!