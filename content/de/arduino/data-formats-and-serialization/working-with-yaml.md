---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:32.291931-07:00
description: "YAML (YAML Ain't Markup Language) ist ein f\xFCr Menschen lesbarer Datenserialisierungsstandard,\
  \ der f\xFCr Konfigurationsdateien, Kommunikation zwischen\u2026"
lastmod: 2024-02-19 22:05:13.098272
model: gpt-4-0125-preview
summary: "YAML (YAML Ain't Markup Language) ist ein f\xFCr Menschen lesbarer Datenserialisierungsstandard,\
  \ der f\xFCr Konfigurationsdateien, Kommunikation zwischen\u2026"
title: Arbeiten mit YAML
---

{{< edit_this_page >}}

## Was & Warum?

YAML (YAML Ain't Markup Language) ist ein für Menschen lesbarer Datenserialisierungsstandard, der für Konfigurationsdateien, Kommunikation zwischen Programmen und Datenspeicherung verwendet werden kann. Programmierer wenden sich bei Arduino-Projekten an YAML, um den Konfigurationsprozess ihrer Anwendungen zu vereinfachen, was es leichter macht, Parameter ohne tiefgehendes Eingreifen in den Code zu ändern, die Lesbarkeit erhöht und das Teilen von Konfigurationen einfacher macht.

## Wie geht das:

Direkt mit YAML auf Arduino zu arbeiten, ist aufgrund von Speicherbeschränkungen und dem Fehlen von nativen YAML-Verarbeitungsbibliotheken nicht so einfach wie in höheren Programmierumgebungen. Doch für Projekte, die das Parsen oder Generieren von YAML erfordern, beinhaltet ein typischer Ansatz die Verwendung eines Begleitrechners (wie ein Raspberry Pi) oder das Konvertieren von YAML-Dateien in ein Arduino-freundlicheres Format (wie JSON) unter Verwendung von externen Skripten. Zu Demonstrationszwecken konzentrieren wir uns auf den letzteren Ansatz unter Verwendung einer beliebten Bibliothek: ArduinoJson.

**Schritt 1:** Konvertieren Sie Ihre YAML-Konfiguration in JSON. Sie können Online-Tools oder Befehlszeilenprogramme wie `yq` verwenden.

YAML-Datei (`config.yaml`):
```yaml
wifi:
  ssid: "YourSSID"
  password: "YourPassword"
```

Konvertiert zu JSON (`config.json`):
```json
{
  "wifi": {
    "ssid": "YourSSID",
    "password": "YourPassword"
  }
}
```

**Schritt 2:** Verwenden Sie die ArduinoJson-Bibliothek, um die JSON-Datei in Ihrem Arduino-Sketch zu parsen. Zuerst müssen Sie die ArduinoJson-Bibliothek über den Bibliotheks-Manager in der Arduino-IDE installieren.

**Schritt 3:** Laden und parsen Sie das JSON in Ihrem Code. Aufgrund der Speicherbeschränkungen von Arduino stellen Sie sich vor, der JSON-String ist in einer Variablen gespeichert oder wird von einer SD-Karte gelesen.

Muster-Arduino-Sketch:
```cpp
#include <ArduinoJson.h>

const char* jsonConfig = "{\"wifi\":{\"ssid\":\"YourSSID\",\"password\":\"YourPassword\"}}";

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc;
  DeserializationError error = deserializeJson(doc, jsonConfig);

  if (error) {
    Serial.print(F("deserializeJson() fehlgeschlagen: "));
    Serial.println(error.f_str());
    return;
  }

  const char* ssid = doc["wifi"]["ssid"]; // "YourSSID"
  const char* password = doc["wifi"]["password"]; // "YourPassword"

  Serial.print("SSID: ");
  Serial.println(ssid);
  Serial.print("Passwort: ");
  Serial.println(password);
}

void loop() {
  // Nichts hier für dieses Beispiel
}
```

Ausgabe beim Ausführen des Skripts:
```
SSID: YourSSID
Passwort: YourPassword
```

Dieser Ansatz, der die Konvertierung zu JSON und die Nutzung der ArduinoJson-Bibliothek beinhaltet, ermöglicht eine handhabbare YAML-Konfigurationsverarbeitung innerhalb von Arduino-Projekten, umgeht das direkte Parsen von YAML auf dem Mikrocontroller.
