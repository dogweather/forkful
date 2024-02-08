---
title:                "Einen Datum aus einem String analysieren"
aliases:
- de/arduino/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:16.775074-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen Datum aus einem String analysieren"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String in Arduino umfasst das Extrahieren und Konvertieren der Datumsbestandteile (Jahr, Monat, Tag) aus einer textuellen Darstellung in ein Format, das für die Zeitmessung, Vergleiche oder Manipulationen innerhalb von Skizzen genutzt werden kann. Programmierer führen diese Aufgabe häufig durch, um mit Komponenten wie Echtzeituhren, Loggern zu interagieren oder um Eingaben von Web-APIs und Benutzeroberflächen zu verarbeiten, bei denen Daten in einem lesbaren Format präsentiert werden könnten.

## Wie:

Direkter Ansatz ohne Drittanbieter-Bibliothek:

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // Beispiel-Datumsstring im YYYY-MM-DD-Format
  String dateString = "2023-04-01"; 

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // Initialisieren eines DateTime-Objekts mit den geparsten Bestandteilen
  DateTime parsedDate(year, month, day);
  
  Serial.print("Geparstes Datum: ");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

Beispielausgabe:
```
Geparstes Datum: 2023/4/1
```

Verwendung einer Drittanbieter-Bibliothek (*ArduinoJson* für komplexere Parsingszenarien, wie das Erhalten eines Datums aus einer JSON-Antwort):

Zuerst installiere die ArduinoJson-Bibliothek über den Arduino-Bibliotheksmanager.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Simulation einer JSON-Antwort
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // Extrahieren des Datumstrings
  const char* date = doc["date"];

  // Das Datum aus dem String parsen wie zuvor
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("Geparstes Datum aus JSON: ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

Beispielausgabe:
```
Geparstes Datum aus JSON: 2023/7/19
```
