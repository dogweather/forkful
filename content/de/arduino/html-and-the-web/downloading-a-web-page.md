---
aliases:
- /de/arduino/downloading-a-web-page/
date: 2024-01-20 17:43:43.645851-07:00
description: "Herunterladen einer Webseite bedeutet, deren Inhalte \xFCber das Internet\
  \ auf dein Ger\xE4t zu laden. Programmierer tun das, um Daten zu sammeln oder Webdienste\u2026"
lastmod: 2024-02-18 23:09:05.144793
model: gpt-4-1106-preview
summary: "Herunterladen einer Webseite bedeutet, deren Inhalte \xFCber das Internet\
  \ auf dein Ger\xE4t zu laden. Programmierer tun das, um Daten zu sammeln oder Webdienste\u2026"
title: Webseite herunterladen
---

{{< edit_this_page >}}

## Was & Warum?
Herunterladen einer Webseite bedeutet, deren Inhalte über das Internet auf dein Gerät zu laden. Programmierer tun das, um Daten zu sammeln oder Webdienste zu nutzen.

## Vorgehensweise:
Hier ein einfaches Beispiel, mit dem Arduino eine Webseite herunterlädt. Nutze dafür das `ESP8266WiFi`-Modul:
```Arduino
#include <ESP8266WiFi.h>

const char* ssid = "DEIN_WIFI_NAME";
const char* password = "DEIN_WIFI_PASSWORT";
const char* host = "example.com";

void setup() {
  Serial.begin(115200);
  // Verbinden mit WLAN
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("WLAN verbunden.");
  
  // Webseite herunterladen
  WiFiClient client;
  const int httpPort = 80;
  if (!client.connect(host, httpPort)) {
    Serial.println("Verbindung fehlgeschlagen.");
    return;
  }
  
  // HTTP-Anfrage senden
  client.print(String("GET /") + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" +
               "Connection: close\r\n\r\n");
  while(client.connected()) {
    String line = client.readStringUntil('\n');
    if (line == "\r") {
      break; // Header sind vorbei, jetzt kommt der Content
    }
  }
  // Content auslesen und anzeigen
  String line = client.readStringUntil('\n');
  Serial.println(line);
}

void loop() {
  // Nichts zu tun hier
}
```
Wenn alles richtig ist, siehst du den Inhalt der Seite `example.com` in deinem Serial Monitor.

## Tiefer Eintauchen:
Der ESP8266 Mikrocontroller hat IoT-Projekte revolutioniert, weil er günstig und einfach zu verwenden ist. Es gibt Alternativen wie den ESP32, der mehr Leistung und zusätzliche Features bietet. Wichtig beim Herunterladen von Webseiten: HTTP/1.1 nutzen, alles andere ist veraltet. SSL/TLS sollte benutzt werden, wenn vertrauliche Daten im Spiel sind – dafür müsste dein Arduino HTTPS unterstützen.

## Siehe auch:
- [ESP8266WiFi Dokumentation](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
- [ESP8266HTTPClient Bibliothek](https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266HTTPClient)
- [ESP8266 Community Forum](https://www.esp8266.com/)
