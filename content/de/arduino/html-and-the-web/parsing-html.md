---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:32.213536-07:00
description: "Das Parsen von HTML in Arduino-Projekten bedeutet, Informationen von\
  \ Webseiten zu extrahieren. Programmierer tun dies, um ihre Arduino-Ger\xE4te zur\u2026"
lastmod: '2024-03-13T22:44:54.142701-06:00'
model: gpt-4-0125-preview
summary: Das Parsen von HTML in Arduino-Projekten bedeutet, Informationen von Webseiten
  zu extrahieren.
title: HTML parsen
weight: 43
---

## Wie geht das:
Das Parsen von HTML auf Arduino verlangt in der Regel nach Bibliotheken mit geringem Speicherbedarf aufgrund der begrenzten Ressourcen des Geräts. Eine beliebte Wahl für Web-Scraping und -Parsing ist die Verwendung der `ESP8266HTTPClient`- und `ESP8266WiFi`-Bibliotheken für den ESP8266 oder deren ESP32-Pendants, aufgrund ihrer nativen Unterstützung für Wi-Fi-Fähigkeiten und HTTP-Protokolle. Hier ist ein einfaches Beispiel, um HTML zu holen und zu parsen, unter der Annahme, dass Sie mit einem ESP8266 oder ESP32 arbeiten:

Zuerst, die notwendigen Bibliotheken einbinden:
```cpp
#include <ESP8266WiFi.h> // Für ESP8266
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// Verwenden Sie die entsprechenden ESP32-Bibliotheken, falls Sie ein ESP32 verwenden

const char* ssid = "IhrSSID";
const char* password = "IhrPASSWORT";
```

Verbinden Sie sich mit Ihrem Wi-Fi-Netzwerk:
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("Verbinden...");
    }
}
```

Machen Sie eine HTTP-Anfrage und parsen Sie ein einfaches Stück HTML:
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { //Überprüfen des WiFi-Verbindungsstatus
        HTTPClient http;  //Deklarieren eines Objektes der Klasse HTTPClient

        http.begin("http://example.com");  //Angeben des Anfrageziels
        int httpCode = http.GET();  //Senden der Anfrage

        if (httpCode > 0) { //Überprüfung des Rückkehr-Codes
            String payload = http.getString();   //Die Antwort der Anfrage erhalten
            Serial.println(payload);             //Die Antwort ausgeben

            // Einen spezifischen Teil parsen, z.B. Extrahieren des Titels aus der Nutzlast
            int titleStart = payload.indexOf("<title>") + 7; // +7, um den "<title>"-Tag zu überspringen
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("Seitentitel: ");
            Serial.println(pageTitle);
        }

        http.end();   //Verbindung schließen
    }

    delay(10000); //Alle 10 Sekunden eine Anfrage stellen
}
```

Beispielausgabe (unter der Annahme, dass http://example.com eine einfache HTML-Struktur hat):
```
Verbinden...
...
Seitentitel: Beispiel-Domain
```

Dieses Beispiel demonstriert das Abrufen einer HTML-Seite und das Extrahieren des Inhalts des `<title>`-Tags. Für komplexeres HTML-Parsing sollten reguläre Ausdrücke (mit Vorsicht aufgrund von Speicherbeschränkungen) oder String-Manipulationsfunktionen in Betracht gezogen werden, um durch die HTML-Struktur zu navigieren. Fortgeschrittenes Parsing könnte anspruchsvollere Ansätze erfordern, einschließlich benutzerdefinierter Parsing-Algorithmen, die auf die spezifische Struktur des zu bearbeitenden HTMLs zugeschnitten sind, da die Standard-Arduino-Umgebung keine integrierte HTML-Parsing-Bibliothek enthält.
