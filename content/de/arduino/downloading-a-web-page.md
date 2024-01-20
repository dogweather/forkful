---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite ist der Prozess, bei dem Daten von einer Webseite auf deinen Lokalspeicher übertragen werden. Programmierer tun dies, um die Daten für Analyse, Überwachung oder Offline-Zugriff zu nutzen.

## So geht's:

```Arduino
#include <Ethernet.h>
#include <SPI.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
char server[] = "www.beispiel.de";
EthernetClient client;

void setup()
{
  Ethernet.begin(mac);
  Serial.begin(9600);

  if (client.connect(server, 80)) {
    client.println("GET / HTTP/1.1");
    client.println("Host: www.beispiel.de");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  if (client.available()) {
    char c = client.read();
    Serial.print(c);
  }
  
  if (!client.connected()) {
    Serial.println();
    Serial.println("disconnecting.");
    client.stop();
    for(;;)
      ;
  }
}
```

Wenn du dieses Skript ausführst, siehst du den HTML-Code der Webseite auf deinem Serial-Monitor.

## Deep Dive:

Historisch gesehen hat die Herstellung von Web-Daten auf automatisierte Weise das Aufkommen von Internet-Bots vorangetrieben. Alternativen zum Arduino für diese Aufgabe sind der Raspberry Pi oder der ESP8266, die beide in der Lage sind, Webseiten herunterzuladen und mehr Verarbeitungsleistung bieten.

Die Implementierung dieser Funktion in Arduino setzt auf die Ethernet-Bibliothek, die die Unterstützung für den Ethernet-Controller des Arduino bereitstellt. Allerdings muss betont werden, dass standardmäßig nur eine begrenzte Anzahl von gleichzeitigen Verbindungen unterstützt wird.

## Siehe auch:

- [Arduino Ethernet Bibliothek](https://www.arduino.cc/en/Reference/Ethernet)
- [Raspberry Pi Web Scraping](https://projects.raspberrypi.org/en/projects/secret-agent-chat)