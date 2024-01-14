---
title:                "Arduino: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Warum

Warum sollte man sich mit dem Senden von HTTP-Anfragen mit grundlegender Authentifizierung beschäftigen? Nun, HTTP-Anfragen sind eine wesentliche Methode, um mit Webservern zu kommunizieren, insbesondere in der Welt des Internet of Things (IoT). Sie ermöglichen es unseren Arduino-Boards, Daten von oder zu einem Server zu senden oder von diesem abzurufen. Mit grundlegender Authentifizierung können wir die Sicherheit unserer Daten erhöhen, indem wir einen Benutzernamen und ein Passwort verwenden, um uns mit dem Server zu authentifizieren.

# Wie geht's

Um eine HTTP-Anfrage mit grundlegender Authentifizierung zu senden, benötigen wir zuerst die IP-Adresse des Servers und die URL, an die wir die Anfrage senden möchten. Dann müssen wir den Benutzernamen und das Passwort angeben, mit denen wir uns authentifizieren möchten. Hier ist ein Beispielcode in Arduino:

```
// Bibliothek für die Verwendung von WiFi
#include <WiFi.h>

// Netzwerksname und Passwort
const char* ssid = "Mein NETZWERK";
const char* password = "password";

// IP-Adresse und Port des Servers
const char* host = "192.168.1.100"; // Beispiel-IP-Adresse
const uint16_t port = 80;

// Benutzername und Passwort für die Authentifizierung
const char* username = "benutzername";
const char* pass = "passwort";

// Eine WiFi-Client-Instanz erstellen
WiFiClient client;

void setup() {
  // Mit dem WiFi-Netzwerk verbinden
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  // WiFi-Verbindung erfolgreich
  Serial.println("Verbunden!");

  // Öffnen Sie eine TCP-Verbindung
  if (!client.connect(host, port)) {
    // Bei Verbindungsfehler
    Serial.println("Verbindung fehlgeschlagen!");
    return;
  }
}

void loop() {
  // Daten, die gesendet werden sollen
  String data = "Hallo, das ist eine Testanfrage";

  // Die HTTP-Anfrage aufbauen
  client.print(String("GET /api/test HTTP/1.1\r\n") +
               "Host: " + host + "\r\n" +
               "Authorization: Basic " + base64.b64encode(username + ":" + pass) + "\r\n" +
               "Content-Length: " + data.length() + "\r\n\r\n" +
               data);

  // Warten auf Antwort vom Server
  delay(500);
  
  // Wenn eine Antwort empfangen wurde
  while (client.available()) {
    // Antwort anzeigen
    String response = client.readStringUntil('\r');
    Serial.println(response);
  }
}

```

Die Ausgabe dieses Codes sollte eine HTTP-Antwort vom Server auf der seriellen Konsole anzeigen. Hier sehen Sie in der Ausgangszeile "Verbunden!" die WiFi-Verbindung wurde erfolgreich hergestellt. Dann sehen wir die geschickte Anfragezeile und die empfangene Antwort vom Server.

# Tiefer tauchen

Um eine grundlegende Authentifizierung durchzuführen, müssen wir den Benutzernamen und das Passwort kodieren und als Teil unseres HTTP-Anfrage-Headers senden. Dies kann mit der Base64-Kodierung geschehen, die in unserem Beispiel durch die "base64" Bibliothek ermöglicht wird. Wenn wir die Anfrage mit einem Packet Sniffer untersuchen, werden wir sehen, dass das passwortgesicherte Datagramm wie folgt aussieht:

```
GET /api/test HTTP/1.1
Host: 192.168.1.100
Authorization: Basic base64(benutzername:passwort)
Content-Length: 29

Hallo, das ist eine Testanfrage
```

Wir können auch feststellen, dass die Base64-Kodierung keine sichere Methode zur Übertragung von Passwörtern ist, da sie relativ einfach zu entschlüsseln ist. Um die Sicherheit zu erhöhen, können wir SSL (Secure Socket Layer) verwenden, um die Kommunikation zwischen dem Arduino und dem Server zu verschlüsseln.

# Siehe auch

- [Offizielle Arduino-Website](https://www.arduino.cc/)
- [WiFi Library Referenz](https://www.arduino.cc/en/Reference/WiFi)
- [Base64 Arduino Bibliothek](https://