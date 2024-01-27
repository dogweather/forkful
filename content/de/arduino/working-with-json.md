---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?

JSON (JavaScript Object Notation) wird für Datenaustausch verwendet, speziell, wenn Daten zwischen einem Server und einem Webclient verschickt werden. Es ist leicht zu lesen und zu schreiben, und von Maschinen einfach zu interpretieren und zu generieren, was die Programmierung effizienter macht.

## Anleitung:

Installiere die `ArduinoJson` Bibliothek im Arduino IDE, dann probiere folgenden Code:

```Arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Ein JSON-Objekt erstellen
  StaticJsonDocument<200> doc;
  doc["sensor"] = "GPS";
  doc["time"] = 1351824120;

  // JSON im String umwandeln
  String output;
  serializeJson(doc, output);
  Serial.println(output);
}

void loop() {
  // Nichts zu tun hier
}
```

Dies wird ein JSON-Objekt erstellen und als String ausgeben:

```
{"sensor":"GPS","time":1351824120}
```

## Tiefergehende Infos:

JSON wurde Anfang der 2000er Jahre entworfen und ist zu einem Standard für APIs und Webdienste geworden. Es hat XML in vielen Anwendungsbereichen ersetzt, weil es leichter und schneller zu parsen ist. Die `ArduinoJson` Bibliothek bietet eine einfache Nutzung und effizientes Speichermanagement unter Arduino.

Alternativen wie MsgPack (MessagePack) existieren, die Daten in einem noch kompakteren Format darstellen, aber JSON bleibt wegen seiner Lesbarkeit und leichteren Handhabung beliebt.

In der Microcontroller-Programmierung müssen Entwickler speicher- und rechenleistungsoptimierten Code schreiben, daher ist ein Verständnis von Speichernutzung und Speicherverwaltung wichtig, wenn man mit JSON auf solchen Systemen arbeitet.

## Siehe Auch:

- ArduinoJson Bibliothek Dokumentation: https://arduinojson.org/
- JSON Spezifikation: https://www.json.org/json-de.html
- Ein Tutorial zu JSON und Arduino: https://lastminuteengineers.com/handling-json-arduino/
