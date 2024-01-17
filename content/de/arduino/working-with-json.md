---
title:                "Arbeiten mit JSON"
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Was und Warum?

JSON ist eine Dateiformat- und Datenübertragungsformat, das von Programmierern verwendet wird, um strukturierte Daten in einer menschenlesbaren Form zu speichern und zu übertragen. Programmierer verwenden JSON, um Daten effizient zu organisieren und auszutauschen, da es einfach zu lesen und zu schreiben ist und von vielen Programmiersprachen unterstützt wird.

## Wie geht's?

Um mit JSON in Arduino zu arbeiten, müssen Sie die ArduinoJSON-Bibliothek installieren. Diese Bibliothek ermöglicht es Ihnen, JSON-Daten zu empfangen, zu verarbeiten und zu erstellen. Hier ist ein Beispielcode, der eine JSON-Datei empfängt und die darin enthaltenen Daten ausgibt:

```
Arduino ...
#include <ArduinoJson.h>    // JSON Bibliothek einbinden

void setup() {
  Serial.begin(9600);       // Serielle Verbindung starten
}

void loop() {
  StaticJsonDocument<200> doc;    // JSON-Dokument erstellen
  deserializeJson(doc, Serial);   // JSON-Dokument aus serial lesen

  const char* name = doc["name"];     // Daten auslesen
  int age = doc["age"];

  Serial.print("Name: ");        // Daten ausgeben
  Serial.println(name);
  Serial.print("Alter: ");
  Serial.println(age);
}
```

Die Ausgabe könnte folgendermaßen aussehen:

```
Name: Max
Alter: 25
```

Mehr Informationen zu den Möglichkeiten mit der ArduinoJSON-Bibliothek finden Sie in der [offiziellen Dokumentation](https://arduinojson.org/).

## Tiefen-Tauchgang

JSON wurde erstmals im Jahr 2001 von Douglas Crockford vorgestellt und ist seitdem ein beliebtes Format bei Entwicklern. Es ist eine Alternative zu älteren Formaten wie XML, da es kompakter und einfacher zu lesen und zu schreiben ist.

Es gibt auch andere Möglichkeiten, mit JSON in Arduino zu arbeiten, wie zum Beispiel die Verwendung der [ESP8266-JSON-Bibliothek](https://github.com/bblanchon/ArduinoJson). Diese Bibliothek ist für den ESP8266-Chip optimiert und bietet zusätzliche Funktionen.

In Bezug auf die Implementierung sollten Sie beachten, dass die ArduinoJSON-Bibliothek nur mit festen Objektstrukturen funktioniert. Wenn Sie JSON-Dateien mit variablen Strukturen verarbeiten müssen, gibt es andere Bibliotheken, die möglicherweise besser geeignet sind.

## Sieh dir auch an

- [ArduinoJSON-Dokumentation](https://arduinojson.org/)
- [ESP8266-JSON-Bibliothek](https://github.com/bblanchon/ArduinoJson)
- [Douglas Crockford über die Geschichte von JSON](https://www.json.org/json-de.html)