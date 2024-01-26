---
title:                "Einen HTTP-Request senden"
date:                  2024-01-20T17:58:51.582090-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen HTTP-Request senden"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen senden bedeutet, mit einem Webserver zu kommunizieren – so holt man Daten oder schickt sie. Programmierer nutzen das, um ihre Arduino-Projekte mit dem Internet zu verbinden, damit sie Informationen austauschen können.

## Wie geht das:
Mit dem ESP8266/ESP32 und der entsprechenden Bibliothek sieht das so aus:

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "DEIN_WIFI_NAME";
const char* password = "DEIN_WIFI_PASSWORT";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Verbindung mit WLAN...");
  }

  if (WiFi.status() == WL_CONNECTED) { 
    HTTPClient http;
    http.begin("http://jsonplaceholder.typicode.com/users/1"); // Deine Ziel-URL
    int httpCode = http.GET();

    if (httpCode > 0) { 
      String payload = http.getString();
      Serial.println(httpCode);
      Serial.println(payload);
    }
    http.end();
  }
}

void loop() {
  // Hier ist nichts weil die HTTP-Anfrage nur einmal in 'setup()' gemacht wird.
}
```

Hier bekommst du eine Ausgabe wie:

```
200
{
  "id": 1,
  "name": "Leanne Graham",
  ...
}
```

`200` ist der HTTP Statuscode für "OK" – deine Anfrage war erfolgreich.

## Tiefere Einblicke
Das Senden von HTTP-Anfragen mit einem Arduino begann mit einfacheren Netzwerkmodulen, wie dem Ethernet Shield. Heute sind Boards wie der ESP8266 und der ESP32 populärer. Sie integrieren WLAN-Funktionalität, was das Ganze kompakter und kostengünstiger macht.

Alternativen zum ESP8266/ESP32 wären etwa der Arduino MKR1000 oder Ethernet-Shields, die allerdings oft komplizierter in der Handhabung sind.

Wichtig bei der Implementierung: Sicherheit. HTTP ohne S (`HTTPS`) ist nicht verschlüsselt. Für sensible Daten also lieber `HTTPS` verwenden. ESP8266/ESP32 unterstützen das mit der `WiFiClientSecure`-Bibliothek.

## Siehe auch
Für mehr Details sieh dir die offizielle ESP8266/ESP32-Dokumentation an. Hier findest du Infos zu Funktionen und Beispielen:

- [ESP8266 Arduino Core Documentation](https://arduino-esp8266.readthedocs.io/en/latest/)
- [ESP32 Arduino Core Documentation](https://docs.espressif.com/projects/arduino-esp32/en/latest/)

Möchtest du tiefer in das Thema einsteigen, sind folgende Ressourcen hilfreich:

- [HTTP-Methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [Arduino Client for MQTT](https://pubsubclient.knolleary.net/)

Einen einfachen Server, um Anfragen zu testen und Beispielausgaben zu sehen, bietet:

- [JSONPlaceholder](https://jsonplaceholder.typicode.com/)
