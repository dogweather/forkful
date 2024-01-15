---
title:                "Senden einer http-Anfrage mit grundlegenden Authentifizierung"
html_title:           "Arduino: Senden einer http-Anfrage mit grundlegenden Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegenden Authentifizierung"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Hast du schon mal darüber nachgedacht, wie du mit deinem Arduino-Gerät eine Verbindung zu einer Website herstellen kannst? Vielleicht möchtest du Daten von einem Server abrufen oder Befehle senden. Eine Möglichkeit dies zu erreichen ist durch das Senden von HTTP Anfragen, die eine Authentifizierung erfordern. In diesem Artikel zeige ich dir, wie du das mit dem aktuellen Arduino machen kannst.

## Wie es geht

Um eine HTTP Anfrage mit grundlegender Authentifizierung zu senden, musst du zuerst deine Netzwerkverbindung einrichten. Dafür brauchst du eine Ethernet-Bibliothek und die IP-Adresse und Port des Servers, zu dem du eine Verbindung herstellen möchtest.

```
Arduino Ethernet.h
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED }; // Beispiel MAC-Adresse
IPAddress serverIP(192, 168, 1, 10); // Beispiel IP-Adresse des Servers
WiFiClient client; // Hiermit stellen wir eine Verbindung zum Server her
int port = 80; // Beispiel Port für HTTP Anfragen
```

Als nächstes musst du deine Anfrage erstellen, indem du den Anfrage-Typ (GET oder POST), die gewünschte URL und den Host des Servers angegeben.

```
String request = "GET /example HTTP/1.1\r\n"; // Beispiel Anfragen-URL
request += "Host: www.example.com\r\n"; // Beispiel Host
```

Sobald du deine Anfrage erstellt hast, musst du die grundlegende Authentifizierung hinzufügen. Dies wird über eine Base64-kodierte Kombination aus Benutzername und Passwort erreicht.

```
String auth = "Benutzername:Passwort"; // Beispiel Benutzername und Passwort
String encodedAuth = base64::encode(auth); // Kodiert in Base64
request += "Authorization: Basic " + encodedAuth + "\r\n"; // Hinzufügen der Authentifizierung zur Anfrage
```

Nachdem du deine HTTP Anfrage erstellt hast, musst du sie nur noch an den Server senden und die Antwort empfangen. Hier ist ein vollständiges Beispiel, das eine Anfrage an einen Server sendet und den Text der Antwort ausgibt.

```
client.connect(serverIP, port); // Verbindung zum Server herstellen
client.print(request); // Anfrage senden
delay(100); // Kurze Verzögerung, um sicherzustellen, dass die Antwort vollständig ist
while (client.available()) { // Antwort empfangen und ausgeben
  String response = client.readStringUntil('\r'); // Text bis zum Zeilenumbruch lesen
  Serial.print(response); // Ausgabe im seriellen Monitor
}
client.stop(); // Verbindung schließen
```

## Tieferer Einblick

Um besser zu verstehen, wie die grundlegende Authentifizierung in einer HTTP Anfrage funktioniert, ist es hilfreich, die genaue Struktur der Anfrage zu verstehen. Eine typische HTTP Anfrage hat die folgende Struktur:

```
Methode URL Protokoll/Version
Kopfzeile 1
Kopfzeile 2
...
Leerzeile
Text der Nachricht (z.B. POST-Daten)
```

Die Methode ist normalerweise GET oder POST, aber es gibt noch andere wie PUT und DELETE. Die URL ist die Adresse der Ressource, die angefordert wird. Die Kopfzeilen enthalten zusätzliche Informationen wie Host, Benutzeragent usw. Die Leerzeile trennt die Kopfzeilen vom restlichen Text der Anfrage.

In unserem Beispiel haben wir die HTTP Methode auf GET und die URL auf /example gesetzt. Wir haben auch die Host-Kopfzeile auf www.example.com gesetzt, damit der Server weiß, an wen die Anfrage gerichtet ist. Die grundlegende Authentifizierung wird durch die Kopfzeile "Authorization" hinzugefügt, gefolgt von der Base64-kodierten Kombination aus Benutzername und Passwort.

## Siehe auch

Wenn du mehr über das Senden von HTTP Anfragen mit deinem Arduino erfahren möchtest, schau dir diese nützlichen Links an:

- [Arduino Ethernet Bibliothek Dokumentation](https://www.arduino.cc/en/reference/Ethernet)
- [Base64 kodieren in Arduino](https://www.arduino.cc