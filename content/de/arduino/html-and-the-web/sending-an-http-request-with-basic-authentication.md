---
date: 2024-01-20 18:01:05.490156-07:00
description: "HTTP-Requests mit Basic Authentication senden Daten an einen Server,\
  \ gesch\xFCtzt durch Benutzername und Passwort. Programmierer nutzen das f\xFCr\
  \ sichere\u2026"
lastmod: '2024-03-11T00:14:28.044948-06:00'
model: gpt-4-1106-preview
summary: "HTTP-Requests mit Basic Authentication senden Daten an einen Server, gesch\xFC\
  tzt durch Benutzername und Passwort. Programmierer nutzen das f\xFCr sichere\u2026"
title: HTTP-Anfragen mit Basisauthentifizierung senden
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Requests mit Basic Authentication senden Daten an einen Server, geschützt durch Benutzername und Passwort. Programmierer nutzen das für sichere Datenübertragungen zu APIs oder Webdiensten.

## Anleitung:
Hier ist ein einfacher Codeabschnitt für das Senden einer HTTP-Anfrage mit Basic Authentication auf einem Arduino.

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>
#include <Base64.h>

const char* ssid = "DEIN_WIFI_NAME";
const char* password = "DEIN_WIFI_PASSWORT";

const char* user = "DEIN_USERNAME";
const char* pass = "DEIN_PASSWORT";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("Verbunden mit WiFi");
  
  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http;
    http.begin("http://deineurl.de/daten"); // URL für die Anfrage
    http.setAuthorization(user, pass);
    int httpCode = http.GET();

    if (httpCode > 0) {
      String payload = http.getString();
      Serial.println(httpCode);
      Serial.println(payload);
    } else {
      Serial.println("Error bei der Anfrage");
    }
    http.end();
  }
}

void loop() {
  // Hier könnte deine Logik stehen
}
```

Die Ausgabe sieht wie folgt aus:

```
Verbunden mit WiFi
200
{"response":"Daten erfolgreich empfangen"}
```

## Tiefere Einblicke:
Historisch basiert die Basic Authentication auf einem einfachen Schema, das im HTTP/1.0 zugefügt wurde. Es kodiert Benutzername und Passwort in Base64, was nicht sicher ist ohne HTTPS, da es leicht entschlüsselt werden kann.

Es gibt sicherere Alternativen wie OAuth oder API-Schlüssel. Basic Authentication wird jedoch wegen ihrer Einfachheit und Unterstützung durch viele Webdienste noch häufig verwendet.

Beim Implementieren auf einem Arduino muss man die entsprechende Bibliothek einsetzen, hier am Beispiel des ESP8266 mit der ESP8266WiFi-Bibliothek. Die Anmeldeinformationen sollten nie im Code hartkodiert sein, besonders in produktiven Umgebungen. Besser wäre die Verwendung von gesicherten Speicheroptionen wie dem EEPROM des Mikrocontrollers.

## Siehe auch:
- HTTP Authentication: Basic and Digest Access Authentication: https://tools.ietf.org/html/rfc2617
- ESP8266 Arduino Core Documentation: https://arduino-esp8266.readthedocs.io/en/latest/
- Base64 Encoding and Decoding: https://en.wikipedia.org/wiki/Base64
- Sichere Speicherung von Anmeldeinformationen auf dem Arduino: http://www.esp8266.com/viewtopic.php?f=32&t=10457
