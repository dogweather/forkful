---
title:                "HTML parsen"
date:                  2024-01-20T15:30:06.911867-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTML-Parsing ist das Auslesen und Verarbeiten von HTML-Code, um dessen Inhalt und Struktur zu verstehen. Programmierer führen dies durch, um Webinhalte für Apps zu extrahieren oder Webseiten zu analysieren.

## So geht's:
Arduinos können keine HTML-Dokumente so wie leistungsstarke PCs parsen, aber sie können einfache Daten verarbeiten. Hier ein minimalistischer Ansatz für HTML-Elemente:

```Arduino
#include <Ethernet.h>
#include <EthernetClient.h>

EthernetClient client;

void setup() {
  Ethernet.begin(/* Deine Netzwerkdetails hier */);
  if (client.connect("example.com", 80)) {
    client.println("GET /index.html HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  while (client.available()) {
    String line = client.readStringUntil('\n');
    if (line.indexOf("<title>") >= 0) {
      int start = line.indexOf("<title>") + 7; // Start nach dem <title> Tag
      int end = line.indexOf("</title>"); // End vor dem Closing Tag
      String pageTitle = line.substring(start, end);
      Serial.println(pageTitle);
      break; // Annahme: nur ein Titel-Tag
    }
  }
  
  if (!client.connected()) {
    client.stop();
  }
}
```

Ausgabe auf dem Serial Monitor:

```
Dein Seitentitel
```

## Tiefgang:
Historisch gesehen sind Mikrocontroller wie Arduino nicht für komplexe Textverarbeitungsaufgaben gedacht. Parsing wurde traditionell von Servern durchgeführt. Alternativen zu Arduino für HTML-Parsing könnten z.B. ein Raspberry Pi mit Python und BeautifulSoup sein. Bei der Implementierung auf dem Arduino sollten nur Textsegmente geparst werden, die klein und vorhersehbar sind, um Speicherüberläufe zu vermeiden.

## Siehe auch:
- [Arduino Ethernet-Bibliothek](https://www.arduino.cc/en/Reference/Ethernet)
- [HTML-Tutorial](https://www.w3schools.com/html/)
- [Python BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/) für komplexere HTML-Parsing-Aufgaben.
