---
title:                "Arduino: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Programmieren auf einem Arduino kann Spaß machen und eine Menge Möglichkeiten bieten. Eine solche Möglichkeit ist das Abrufen des aktuellen Datums. Das kann hilfreich sein, um beispielsweise eine Uhrzeit zu synchronisieren oder für Datenerfassungsprojekte.

## Anleitung

Um das aktuelle Datum und die Uhrzeit auf Ihrem Arduino abzurufen, müssen Sie zuerst die Zeitbibliothek hinzufügen. Öffnen Sie dazu einfach die Arduino-IDE und gehen Sie zu `Sketch > Bibliothek einbinden > Zeit`. Wenn die Bibliothek einmal hinzugefügt wurde, können Sie die `Zeit`-Funktionen in Ihrem Code nutzen.

```Arduino
#include <Time.h>

void setup() {
  // Weise die Zeitfunktionen dem seriellen Monitor zu
  Serial.begin(9600);
  
  // Warte, bis der serielle Monitor bereit ist
  while(!Serial) {}
  
  // Setze die Zeitzone auf MESZ (+2) 
  setTime(12, 00, 00, 01, 01, 2021);
}

void loop() {
  // Rufe die aktuelle Zeit ab
  int hour = hour();
  int minute = minute();
  int second = second();
  
  // Gib die Zeit auf dem seriellen Monitor aus
  Serial.print("Es ist jetzt: ");
  Serial.print(hour);
  Serial.print(":");
  Serial.print(minute);
  Serial.print(":");
  Serial.println(second);
  
  // Warte eine Sekunde
  delay(1000);
}
```

Wenn Sie nun den Code auf Ihren Arduino hochladen und den seriellen Monitor öffnen, sollten Sie die aktuelle Uhrzeit angezeigt bekommen.

## Deep Dive

Die `Zeit`-Bibliothek verwendet die interne Uhr des Arduino oder einen externen Echtzeituhren-Modul, um die aktuelle Zeit zu bestimmen. Sie können die Zeit auch von externen Quellen wie NTP-Servern synchronisieren, um genauere Ergebnisse zu erhalten.

Es gibt auch fortgeschrittenere Funktionen in der `Zeit`-Bibliothek, wie das Festlegen von Alarmen und das Zählen der vergangenen Sekunden seit einem bestimmten Zeitpunkt.

## Siehe auch

- [Zeitbibliothek für Arduino](https://www.arduino.cc/en/Reference/Time)
- [Tutorial: Echtzeituhr für Arduino](https://www.instructables.com/id/Real-Time-Clock-RTC-for-Arduino/)
- [NTP-Synchronisation für Arduino mit ESP8266](https://randomnerdtutorials.com/esp8266-nodemcu-date-time-ntp-client-server-arduino/)