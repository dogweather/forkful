---
title:                "Senden einer http-Anfrage"
html_title:           "Arduino: Senden einer http-Anfrage"
simple_title:         "Senden einer http-Anfrage"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Du hast schon öfter von HTTP-Anfragen gehört, fragst dich aber, was genau sie sind und wie du sie nutzen kannst? In diesem Artikel erfährst du, warum du mit Arduino HTTP-Anfragen senden solltest und wie du es machen kannst.

## Wie geht's

Einfacher als du denkst! Hier ist ein Beispielcode, um eine HTTP-Anfrage an eine beliebige Website zu senden:

```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

// Einstellungen für deine WiFi-Verbindung
const char *ssid = "DEIN_WIFI_NAME";
const char *password = "DEIN_WIFI_PASSWORT";

void setup() {

  Serial.begin(115200);

  // Router-Verbindung aufbauen
  WiFi.begin(ssid, password);

  // Warten bis verbunden
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Verbunden...");
  }

  // Neue HTTPClient-Instanz erstellen
  HTTPClient http;

  // Ziel-URL und Anfrage definieren
  http.begin("http://beispielwebsite.com");
  int httpCode = http.GET();

  // Antworte-Code prüfen
  if (httpCode > 0) {
    // Antwort ausgeben
    Serial.print("Antwort-Code: ");
    Serial.println(httpCode);
    // Data ausgeben
    String response = http.getString();
    Serial.println(response);
  } else {
    Serial.println("Fehler beim Verbinden");
  }

  // Verbindung beenden
  http.end();
}

void loop() {

}
```

Die Ausgabe des obigen Codes in der Serial Monitor sollte ähnlich aussehen:

```
Verbunden...
Antwort-Code: 200
<!doctype html>
<html>
<head>
  <!-- Einige HTML-Daten ... -->
</head>
<body>
  <!-- Einige HTML-Daten ... -->
</body>
</html>
```

Und voilà, du hast erfolgreich eine HTTP-Anfrage gesendet und die Antwort empfangen! Natürlich kannst du den Code anpassen, um unterschiedliche HTTP-Methoden (wie POST oder PUT) zu nutzen und die Anfrage an deine Bedürfnisse anzupassen.

## Deep Dive

Eine HTTP-Anfrage zu senden bedeutet, eine Kommunikation mit einem Server herzustellen und eine Anfrage für Daten zu senden. Der Server antwortet dann mit einem Antworte-Code (z.B. 200 für OK oder 404 für Not Found) und den entsprechenden Daten. In unserem Beispiel haben wir den HTTPClient von Arduino verwendet, der es uns ermöglicht, eine Verbindung aufzubauen und eine GET-Anfrage zu senden.

Es gibt jedoch auch andere Möglichkeiten, eine HTTP-Anfrage mit Arduino zu senden, wie die Verwendung von WiFiClient oder ESP8266HTTPClient. Sie alle haben ihre Besonderheiten, aber das Grundprinzip bleibt dasselbe - eine Verbindung herstellen, eine Anfrage senden und auf die Antwort warten. Es lohnt sich, ein wenig zu experimentieren und verschiedene Bibliotheken auszuprobieren, um die für dich am besten geeignete Lösung zu finden.

In unserem Beispiel haben wir eine GET-Anfrage verwendet, aber je nachdem, welche Daten du senden oder abrufen möchtest, kannst du auch andere Methoden wie POST, PUT oder DELETE nutzen. HTTP-Anfragen ermöglichen es dir, eine Vielzahl von Aktionen auf Websites oder Webanwendungen auszuführen, wie z.B. Daten abrufen, Daten aktualisieren oder sogar eine Webseite zu erstellen.

## Siehe auch

Um mehr über die verschiedenen Möglichkeiten der HTTP-Kommunikation mit Arduino zu erfahren, kannst du dir folgende Ressourcen ansehen:

- [Arduino HTTPClient Library](https://www.arduino.cc/en/Reference/HTTPClient)
- [ESP8266HTTPClient Library](https://github.com/esp8266/Arduino/blob/master/libraries/ESP8266HTTPClient/src/ESP8266HTTPClient.h)
- [WiFiClient Library](https://www.arduino.cc/en/Reference/WiFiClient)
- [Interaktive Lektion zum Thema HTTP-Anfragen mit Arduino](https://create.arduino.cc/projecthub/arduino_yun/http-get-with-arduino-yun-7497f8)