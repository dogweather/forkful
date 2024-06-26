---
date: 2024-01-20 17:43:32.772197-07:00
description: "G\xF6r s\xE5 h\xE4r: Tidigare anv\xE4nde Arduino Ethernet Shield f\xF6\
  r n\xE4tverksuppkoppling, men nu \xE4r Wi-Fi-moduler som ESP8266 och ESP32 popul\xE4\
  ra. De \xE4r\u2026"
lastmod: '2024-04-05T22:50:52.475644-06:00'
model: gpt-4-1106-preview
summary: "Tidigare anv\xE4nde Arduino Ethernet Shield f\xF6r n\xE4tverksuppkoppling,\
  \ men nu \xE4r Wi-Fi-moduler som ESP8266 och ESP32 popul\xE4ra."
title: "H\xE4mta en webbsida"
weight: 42
---

## Gör så här:
```Arduino
#include <ESP8266WiFi.h>

const char* ssid = "DITT_WIFI_NAMN";
const char* password = "DITT_LÖSENORD";
const char* host = "example.com";

WiFiClient client;

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("Ansluten till WiFi!");

  if (!client.connect(host, 80)) {
    Serial.println("Anslutning misslyckades");
    return;
  }

  client.print(String("GET /") + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" +
               "Connection: close\r\n\r\n");

  while(client.available() == 0) {
    if (!client.connected()) {
      Serial.println("Servern kopplade ifrån");
      client.stop();
      return;
    }
  }

  while(client.available()) {
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
}

void loop() {
  // Här kör vi ingenting
}
```
Utskrift: 
```
HTTP/1.1 200 OK
[...webbsidans HTML innehåll...]
```

## Fördjupning
Tidigare använde Arduino Ethernet Shield för nätverksuppkoppling, men nu är Wi-Fi-moduler som ESP8266 och ESP32 populära. De är kostnadseffektiva och innehåller inbyggt Wi-Fi. 
Alternativ finns också: HTTP-klientbibliotek som 'HTTPClient' för ESP kan förenkla processen.
Viktigt är att hantera anslutning och att skicka korrekta HTTP-headrar. Svar skickas som rå text och kan behöva tolkas för att användas.

## Se också
- [ESP8266WiFi bibliotek](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
- [Arduino Ethernet Shield](https://www.arduino.cc/en/Main/ArduinoEthernetShield)
