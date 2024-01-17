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

# Was & Warum?
Webseiten sind essentiell für jegliche Art von Informationsaustausch im Internet. Programmierer nutzen häufig die Möglichkeit, Webseiten herunterzuladen, um die darin enthaltene Information für ihre Projekte zu nutzen.

# Wie geht's?
Für das Herunterladen einer Webseite auf einem Arduino gibt es zwei Hauptmethoden: HTTP-Anfragen und das Verwenden von Bibliotheken. Hier sind Beispiele für jede Methode:

## HTTP-Anfragen:
```arduino
// Bevor wir starten, stellen Sie sicher, dass Sie das Ethernet-Shield an Ihren Arduino angeschlossen haben.

// Bibliotheken einbinden
#include <SPI.h>
#include <Ethernet.h>

// Ethernet-Objekt erstellen
EthernetClient client; 

void setup() {
  // Starten Sie die serielle Kommunikation um die Ausgabe zu sehen
  Serial.begin(9600);
  
  // Stellt Verbindung zum Server her
  Serial.println("Verbinde zu Server...");
  if (client.connect("www.example.com", 80)) {
    // HTTP-Anfrage senden
    client.println("GET /index.html HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
  } else {
    // Fehlerbehandlung, falls keine Verbindung hergestellt werden kann
    Serial.println("Verbindung fehlgeschlagen");
    while(1);
  }
}

void loop() {
  // Auf Antwort vom Server warten
  if (client.available()) {
    // Ausgabe der empfangenen Daten
    char c = client.read();
    Serial.print(c);
  }

  // Wenn keine Daten mehr empfangen werden, Verbindung schließen
  if (!client.connected() && !client.available()) {
    Serial.println();
    Serial.println("Verbindung getrennt");
    client.stop();
    while(1);
  }
}
```

## Bibliotheken:
```arduino
// Bevor wir starten, stellen Sie sicher, dass Sie das Ethernet-Shield an Ihren Arduino angeschlossen haben.

// Bibliotheken einbinden
#include <SPI.h>
#include <Ethernet.h>
#include <EthernetClient.h>

// Ethernet-Objekt erstellen und variablen deklarieren
EthernetClient client;
char server[] = "www.example.com";
char path[] = "/index.html";

void setup() {
  // Starten Sie die serielle Kommunikation um die Ausgabe zu sehen
  Serial.begin(9600);
  
  // Stellt Verbindung zum Server her
  Serial.println("Verbinde zu Server...");
  if (client.connect(server, 80)) {
    // HTTP-Anfrage senden
    Serial.println("HTTP-Anfrage senden...");
    client.print("GET ");
    client.print(path);
    client.println(" HTTP/1.1");
    client.print("Host: ");
    client.println(server);
    client.println("Connection: close");
    client.println();
  } else {
    // Fehlerbehandlung, falls keine Verbindung hergestellt werden kann
    Serial.println("Verbindung fehlgeschlagen");
    while(1);
  }
}

void loop() {
  // Auf Antwort vom Server warten
  if (client.available()) {
    // Ausgabe der empfangenen Daten
    char c = client.read();
    Serial.print(c);
  }

  // Wenn keine Daten mehr empfangen werden, Verbindung schließen
  if (!client.connected() && !client.available()) {
    Serial.println();
    Serial.println("Verbindung getrennt");
    client.stop();
    while(1);
  }
}
```

# Tiefere Einblicke
Das Herunterladen von Webseiten auf dem Arduino wird oft in Projekten genutzt, die eine externe Quelle von Informationen benötigen. Die Verwendung von Bibliotheken erleichtert die Implementierung, aber es ist auch möglich, HTTP-Anfragen direkt zu senden. Wenn Sie mehr über das Senden von HTTP-Anfragen mit dem Arduino erfahren möchten, empfehle ich Ihnen die offizielle Ethernet Library Dokumentation von Arduino zu lesen.

# Siehe auch
- [Offizielle Ethernet Library Dokumentation](https://www.arduino.cc/en/reference/ethernet)
- [HTTP Anfrage Beispiel von Sparkfun](https://www.sparkfun.com/tutorials/329)
- [Web-Client Beispiel von Arduino](https://www.arduino.cc/en/Tutorial/WebClient)