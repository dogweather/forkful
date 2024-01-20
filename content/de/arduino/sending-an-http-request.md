---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

HTTP-Anfragen senden bedeutet, mit einem Server zu kommunizieren, um Daten zu senden oder abzurufen. Programmiere das, wenn du mit deinem Arduino im Internet Daten austauschen willst. 

## So geht's:

Hier ist ein einfacher Arduino-Sketch, der eine HTTP-Anfrage an einen Server sendet. Für unser Beispiel benutzen wir die Ethernet-Bibliothek (Ethernet.h).

```Arduino
#include <Ethernet.h>
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
byte server[] = { 93, 184, 216, 34 }; // example.com
byte MyIP[] = {192, 168, 1, 177};

EthernetClient client;

void setup() {
Ethernet.begin(mac, MyIP);
if (client.connect(server, 80)) {
  client.println("GET / HTTP/1.1");
  client.println("Host: example.com");
  client.println("Connection: close");
  client.println();
} else {
  // Failed to connect
}
}

void loop() {
if (client.available()) {
  char c = client.read();
  Serial.print(c);
}
if (!client.connected()) {
  client.stop();
}
}
```

## Tiefere Einblicke

Früher gab es keine HTTP-Anfragen. Seit der Erfindung des Internets wurde das HTTP-Protokoll entwickelt, das den Austausch von Informationen im Web standardisiert hat. Ein alternatives Protokoll zu HTTP ist HTTPS, das dieselben Funktionen bietet, aber zusätzlich eine Verschlüsselung integriert hat. Tatsächlich ist der Code, um eine HTTP-Anfrage zu senden, in der Ethernet-Bibliothek der Arduino-IDE bereits eingebaut, was den Prozess stark vereinfacht.

## Mehr dazu

Wenn du mehr über Arduinos und HTTP-Anfragen lernen möchtest, hier sind einige hilfreiche Links:

- Arduino Ethernet-Bibliothek Dokumentation: [Link](https://www.arduino.cc/en/reference/ethernet)
- HTTP-Protokollspezifikationen: [Link](https://tools.ietf.org/html/rfc2616)
- Tutorial zum Senden von HTTP-Anfragen mit Arduino: [Link](https://startingelectronics.org/tutorials/arduino/ethernet-shield-web-server-tutorial/SD-card-GET-requests/)