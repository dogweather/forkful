---
title:                "Das Senden einer HTTP-Anfrage"
html_title:           "Arduino: Das Senden einer HTTP-Anfrage"
simple_title:         "Das Senden einer HTTP-Anfrage"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Senden einer HTTP-Anfrage ist ein grundlegender Prozess, bei dem ein Programm Anfragen an einen Server sendet, um Daten zu empfangen oder zu senden. Programmierer verwenden dies, um mit verschiedenen Anwendungen und Internetdiensten zu interagieren, z. B. um Daten von Websites abzurufen oder IoT-Geräte zu steuern.

## Wie geht's?
Um eine HTTP-Anfrage in Arduino zu senden, müssen wir zuerst eine Verbindung zum Internet herstellen. Dies kann mit einer Wi-Fi-Shield- oder Ethernet-Karte erfolgen. Anschließend müssen wir eine HTTP-Anfrage erstellen und die URL der gewünschten Ressource angeben. Wir können auch Parameter und Header hinzufügen, je nach Bedarf. Hier ist ein Beispielcode, der eine GET-Anfrage an eine API sendet und die empfangene Antwort auf der seriellen Schnittstelle ausgibt:

```Arduino
#include <WiFi.h>
#include <WiFiClient.h>

void setup() {
  // Wi-Fi-Verbindung herstellen
  WiFi.begin("SSID", "PASSWORT");

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
  }

  // HTTP-Anfrage erstellen
  WiFiClient client;
  // URL der Ressource angeben
  client.get("https://beispiel-api.com/daten");
  
  // Antwort des Servers auf der seriellen Schnittstelle ausgeben
  while (client.available()) {
    Serial.write(client.read());
  }
}

void loop() {
  // Code für weitere Aufgaben
}
```

## Tiefer gehen
Das Senden von HTTP-Anfragen hat eine lange Geschichte und spielt eine wichtige Rolle in der Entwicklung des Internets und der Kommunikationsstandards. Es gibt auch alternative Methoden, um mit Anwendungen oder Geräten über das Internet zu kommunizieren, z. B. MQTT oder das CoAP-Protokoll. Die Implementierung einer HTTP-Anfrage erfordert die Kenntnis der entsprechenden Protokolle und deren Syntax.

## Siehe auch
Weitere Informationen zu HTTP-Anfragen in Arduino finden Sie in der offiziellen Dokumentation: https://www.arduino.cc/en/Reference/HTTPClient
Oder nehmen Sie an der aktiven Community teil und erfahren Sie mehr über Projekte, die das Senden von HTTP-Anfragen nutzen: https://forum.arduino.cc/