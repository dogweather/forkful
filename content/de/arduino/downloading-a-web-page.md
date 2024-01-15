---
title:                "Herunterladen einer Webseite"
html_title:           "Arduino: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Das Herunterladen einer Webseite mit einem Arduino kann für verschiedene Projekte nützlich sein, besonders wenn das Gerät über eine Internetverbindung verfügt. Es ermöglicht dem Nutzer, Informationen aus dem Internet abzurufen und in sein Projekt zu integrieren.

## Anleitung

Um mit einem Arduino eine Webseite herunterzuladen, benötigen Sie eine Ethernet Shield-Erweiterung und die entsprechenden Bibliotheken. Zuerst müssen Sie die Ethernet-Bibliothek in Ihrem Code einbinden:

```Arduino
#include <Ethernet.h>
```

Anschließend müssen Sie eine Ethernet-Verbindung mit Ihrem Netzwerk herstellen. Dazu müssen Sie die IP-Adresse, die Subnetzmaske, das Standard-Gateway und die MAC-Adresse Ihres Arduinos angeben:

```Arduino
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED }; // MAC-Adresse des Arduino
IPAddress ip(192, 168, 1, 177); // IP-Adresse des Arduino
IPAddress subnet(255, 255, 255, 0); // Subnetzmaske des Netzwerks
IPAddress gateway(192, 168, 1, 1); // Standard-Gateway des Netzwerks

Ethernet.begin(mac, ip, gateway, subnet); // Verbindung herstellen
```

Sobald die Verbindung besteht, können Sie die EthernetClient-Bibliothek verwenden, um eine HTTP-Verbindung zu einer Website herzustellen. Dazu müssen Sie die URL der Seite und die TCP-Portnummer angeben:

```Arduino
EthernetClient client; // EthernetClient-Objekt erstellen
client.connect("www.beispielseite.com", 80); // Verbindung zur Seite herstellen
```

Als nächstes müssen Sie eine HTTP-Anfrage senden. Die genaue Anfrage hängt von der Webseite ab, die Sie herunterladen möchten. Eine einfache Anfrage mit der Seite "/index.html" sieht zum Beispiel so aus:

```Arduino
client.println("GET /index.html HTTP/1.1"); // HTTP-Anfrage senden
client.println("Host: www.beispielseite.com"); // Host-Header angeben
client.println("Connection: close"); // Verbindung schließen
client.println(); // Leerzeile
```

Nachdem die Anfrage gesendet wurde, können Sie die empfangenen Daten lesen. Dazu müssen Sie eine Schleife erstellen und jedes empfangene Zeichen einzeln auslesen. Wenn die Verbindung geschlossen wird, ist die Schleife zu beenden:

```Arduino
while (client.connected()) { // Solange die Verbindung besteht
  if (client.available()) { // Wenn Daten empfangen wurden
    char c = client.read(); // Nächstes Zeichen lesen
    Serial.print(c); // Zeichen ausgeben
  }
}

client.stop(); // Verbindung schließen
```

In diesem Beispiel werden die empfangenen Daten einfach auf der seriellen Konsole ausgegeben. Sie können sie aber auch in einer Variablen speichern oder weiterverarbeiten. Beachten Sie auch, dass viele Webseiten HTML-Tags und andere zusätzliche Inhalte enthalten, die Sie ggf. filtern müssen, um an die gewünschten Informationen zu gelangen.

## Tiefgründig

Der oben beschriebene Ansatz ist sehr einfach und eignet sich für einfache Projekte oder zum Lernen. Wenn Sie jedoch komplexere Anfragen senden oder mehr Kontrolle über die Verbindung haben möchten, sollten Sie sich mit der HTTPClient-Bibliothek auseinandersetzen. Diese bietet zusätzliche Funktionen wie die Möglichkeit, Anfrage-Header anzupassen oder SSL-Verbindungen herzustellen.

Sie können auch versuchen, verschiedene APIs zu nutzen, die es Ihnen ermöglichen, Daten von dynamischen Webseiten abzurufen. Ein Beispiel ist das "Simple HTML DOM" Paket, das die Verarbeitung und Extraktion von Daten aus HTML erleichtert.

## Siehe auch

- [Offizielle Arduino Ethernet-Library Dokumentation](https://www.arduino.cc/en/Reference/Ethernet)
- [HTTPClient Bibliothek](https://github.com/amcewen/HttpClient)
- [Simple HTML DOM Paket](https://github.com/shumatech/Arduino-Websocket)