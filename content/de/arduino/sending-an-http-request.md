---
title:                "Arduino: Versenden einer HTTP-Anfrage"
simple_title:         "Versenden einer HTTP-Anfrage"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Das Senden von HTTP-Anfragen kann für diejenigen, die Arduino programmieren, von großer Bedeutung sein, da es ermöglicht, Informationen von externen Servern abzurufen und Aktionen auszulösen. Dies kann zum Beispiel für die Abfrage von Wetterdaten oder das Aktualisieren von Sensordaten von externen Quellen nützlich sein.

## Wie es geht

Um eine HTTP-Anfrage mit Arduino durchzuführen, müssen Sie zunächst die Bibliothek "WiFiClient" importieren. Dann können Sie mit der Funktion "connect" eine Verbindung zu einem externen Server herstellen, indem Sie die IP-Adresse und den Port angeben. Anschließend können Sie mit der Funktion "println" die gewünschte Anfrage an den Server senden. Hier ist ein einfaches Beispiel:

```Arduino
#include <WiFiClient.h>

WiFiClient client;

void setup() {
  Serial.begin(9600);
  WiFi.begin("WifiSSID", "WifiPassword");
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("\nWiFi connected");
  client.connect("192.168.0.1", 80); // Beispiel IP-Adresse und Port
  client.println("GET / HTTP/1.1"); // Beispielanfrage
  client.println();
}

void loop() {
  if (client.available()) {
    while (client.available()) {
      char c = client.read();
      Serial.print(c); // Ausgabe der Antwort des Servers
    }
  }
}
```

Die Ausgabe des obigen Beispiels könnte wie folgt aussehen: 

```
ƒHTTP/1.1 200 OK
Content-Type: text/html
Connection: Closed

<html> 
<head> 
  <title>Arduino Blog</title> 
</head> 
<body>  
  <h1>Willkommen auf dem Arduino Blog!</h1> 
  <p>Hier finden Sie nützliche Tipps und Tutorials rund um die Programmierung von Arduino-Boards.</p> 
</body> 
</html>
```

## Tiefer eintauchen

Um tiefer in das Thema HTTP-Anfragen mit Arduino einzusteigen, können Sie sich näher mit den verschiedenen Befehlen und Funktionen beschäftigen. Zum Beispiel können Sie mit "client.print" anstatt "client.println" die Anfrage als einzelne Bytes senden, was nützlich sein kann, wenn Sie genauer steuern möchten, was gesendet wird. Außerdem können Sie die Verbindung zum Server mit "client.connected" überprüfen und die Antwort des Servers mit "client.peek" vor dem Lesen der Daten überprüfen.

## Siehe auch

- [Offizielle WiFiClient-Dokumentation](https://www.arduino.cc/en/Reference/WiFiClient)
- [Tutorial: ESP8266 mit dem Internet verbinden und HTTP-Anfragen senden](https://randomnerdtutorials.com/esp8266-web-server/) 
- [Beispielprojekt: Wetterstation mit Arduino und Wetterdaten-API](https://www.instructables.com/id/How-to-Make-an-Arduino-Weather-Station-With-WiFi-E/)