---
date: 2024-01-20 17:43:29.550898-07:00
description: "Hvordan gj\xF8r man det: Historisk har Arduino-enheter v\xE6rt begrenset\
  \ til h\xE5ndtering av sensorer og aktuatorer uten nettforbindelse. Med introduksjonen\
  \ av\u2026"
lastmod: '2024-04-05T22:50:55.063806-06:00'
model: gpt-4-1106-preview
summary: "Historisk har Arduino-enheter v\xE6rt begrenset til h\xE5ndtering av sensorer\
  \ og aktuatorer uten nettforbindelse."
title: Nedlasting av en nettside
weight: 42
---

## Hvordan gjør man det:
```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

// Erstatt med ditt nettverksnavn og passord
const char* ssid = "DITT_SSID";
const char* password = "DITT_PASSORD";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Kobler til WiFi...");
  }
  
  Serial.println("WiFi tilkoblet.");

  if(WiFi.status() == WL_CONNECTED) {
    HTTPClient http;
    http.begin("http://example.com"); // Adresse til nettsiden du vil laste ned
    int httpCode = http.GET();

    if(httpCode > 0) {
        String payload = http.getString();
        Serial.println(payload);
    } else {
        Serial.println("Feil under nedlasting av siden.");
    }

    http.end();
  }
}

void loop() {
  // Ikke noe behov for kode her for nå.
}
```

Sample Output:
```
<!doctype html>
<html>
<head>
    <title>Eksempel Nettside</title>
...
```

## Dypdykk
Historisk har Arduino-enheter vært begrenset til håndtering av sensorer og aktuatorer uten nettforbindelse. Med introduksjonen av nettverksmoduler som ESP8266 og ESP32, er nedlasting av nettsider blitt vanlig. Alternative metoder inkluderer bruk av Ethernet-shields eller andre Wi-Fi-moduler. En viktig implementeringsdetalj er håndtering av HTTP-responser og sikring mot ugyldige nettsideadresser for å unngå feil i programmet.

## Se Også
- Arduino's "WiFi"-bibliotekdokumentasjon: https://www.arduino.cc/en/Reference/WiFi
- HTTPClient-biblioteket for ESP32: https://github.com/espressif/arduino-esp32/tree/master/libraries/HTTPClient
- En guide for HTTP-forespørsler i Arduino: https://www.arduino.cc/en/Tutorial/LibraryExamples/HttpClient
- Introduksjon til ESP8266 og ESP32: https://randomnerdtutorials.com/projects-esp8266/
