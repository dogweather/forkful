---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage mit Basisauthentifizierung ist eine Methode, bei der vor dem Zugriff auf bestimmte Web-Server Informationen validiert werden. Programmierer greifen darauf zurück, um eine zusätzliche Sicherheitsebene zu implementieren und unautorisierten Zugriff zu verhindern.

## So Geht's:

Die folgenden Codebeispiele im ```Arduino ... ``` Stil zeigen, wie man eine HTTP-Anfrage mit Basisauthentifizierung sendet.

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>
 
const char* ssid = "your_SSID";
const char* password =  "your_PASSWORD";
 
void setup() {
  WiFi.begin(ssid, password);
 
  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting...");
  }
}
 
void loop() {
  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http;
    
    http.begin("http://yourserver.com");
    http.addHeader("Content-Type", "application/x-www-form-urlencoded");
    http.setAuthorization("username", "password");
    
    int httpCode = http.POST("key=value");
    
    String payload = http.getString();
    
    Serial.println(httpCode);
    Serial.println(payload);
    
    http.end();
  }
  delay(30000);
}
```

Es verbindet sich zuerst mit dem WLAN. Danach sendet es eine HTTP POST Anfrage an "http://yourserver.com". Die Anfrage enthält die Basisauthentifizierung in der Form "username" und "password". Schließlich gibt sie den HTTP-Statuscode aus und liest die Antwort vom Server.

## Tiefere Einblicke:

Historisch gesehen wurde die Basisauthentifizierung für den Webserver-Betrieb eingeführt und bis heute beibehalten. Es ist allerdings anzumerken, dass diese Methode heutzutage als unsicher betrachtet wird. Alternativ bietet sich für sicherheitskritische Anwendungen die Verwendung des OAuth-Verfahrens an. Beim Senden einer HTTP-Anfrage mit Basisauthentifizierung `(http.setAuthorization("username", "password");)` verschlüsselt das Arduino ESP8266-Board die Anmeldedaten und sendet sie im HTTP-Header.

## Siehe Auch:

Weitere Informationen zum Senden einer HTTP-Anfrage mit Basisauthentifizierung finden Sie hier:

- [RFC 7617 (Basisauthentifizierung)](https://tools.ietf.org/html/rfc7617)
- [OAuth](https://de.wikipedia.org/wiki/OAuth)