---
title:                "Senden einer http-Anfrage mit Grundauthentifizierung"
html_title:           "Arduino: Senden einer http-Anfrage mit Grundauthentifizierung"
simple_title:         "Senden einer http-Anfrage mit Grundauthentifizierung"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

#

## Was & Warum?

Das Senden von HTTP-Anfragen mit basic authentication ist eine Methode, um Daten sicher zwischen zwei Geräten zu übertragen. Programmiere nutzen dies, um beispielsweise Daten von Sensoren an einen Server zu senden oder um auf geschützte Geräte zuzugreifen.

## Wie geht's?

### GET-Anfrage mit basic authentication senden:
```
ArduinoClient client;
client.begin("https://www.example.com/"); //<-- Ersetze mit gewünschter URL
client.setAuthorization("username", "password"); //<-- Ersetze mit gewünschtem Benutzernamen und Passwort
client.sendRequest("GET"); //Ersetze "GET" mit gewünschter Methode, z.B. "POST" oder "PUT"
Serial.println(client.getResponse()); //Anzeige der Server-Antwort
```

### Server-Abfrage und Verarbeitung der Antwort:
```
//Überprüfe, ob Server-Antwort erfolgreich war
if (client.success()) {
  String response = client.getResponse(); //Speichere Server-Antwort in Variable
  Serial.println(response); //Gib Server-Antwort auf dem Monitor aus
  //Weitere Verarbeitung der Antwort möglich
} else {
  //Fehlerbehandlung, z.B. 
  Serial.println("Fehler bei der Anfrage");
}
```

## Tiefergehende Informationen:

### Historischer Kontext:
Das Senden von HTTP-Anfragen mit basic authentication ist eine lange bekannte Methode, um Daten sicher über das Internet zu übertragen. Die basic authentication Methode wurde bereits im Jahr 1999 in der RFC 2617 spezifiziert.

### Alternativen:
Es gibt mittlerweile diverse Alternativen zur basic authentication Methode, die aus Sicherheitsgründen bevorzugt werden. Dazu gehören beispielsweise OAuth und JWT (JSON Web Tokens).

### Implementierungsdetails:
Die ArduinoClient Bibliothek bietet eine einfache Möglichkeit, um HTTP-Anfragen mit basic authentication zu senden. Dabei werden Benutzername und Passwort in einem Base64-kodierten Header an den Server geschickt.

## Siehe auch:

- Offizielle ArduinoClient Bibliotheksdokumentation: https://arduino.cc/reference/en/libraries/arduinoclient/
- RFC 2617 zur basic authentication: https://tools.ietf.org/html/rfc2617