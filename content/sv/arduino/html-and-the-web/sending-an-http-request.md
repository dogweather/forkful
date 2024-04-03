---
date: 2024-01-20 17:59:04.638473-07:00
description: "Hur man g\xF6r: ."
lastmod: '2024-03-13T22:44:38.165009-06:00'
model: gpt-4-1106-preview
summary: .
title: "Skicka en http-f\xF6rfr\xE5gan"
weight: 44
---

## Hur man gör:
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "dittSSID";
const char* password = "dittLösenord";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Ansluter till WiFi...");
  }

  Serial.println("Ansluten!");
  
  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http;
    http.begin("http://httpbin.org/ip");  
    int httpCode = http.GET();

    if (httpCode > 0) {
      String payload = http.getString();
      Serial.println(httpCode);
      Serial.println(payload);
    } else {
      Serial.println("Fel vid anslutning");
    }
    http.end();
  }
}

void loop() {
  // ingenting här
}
```
Sample Output:
```
200
{
  "origin": "Din.IP.Adress"
}
```

## Djupdykning
HTTP-begäran kan spåra sina rötter till 1990-talets webbutveckling. Alternativ till ESP8266 för Arduino inkluderar Ethernet Shield och andra Wi-Fi-moduler som ESP32. Vid användning av `HTTPClient`, kom ihåg att hantera anslutningar noggrant för att undvika minnesläckor.

## Se Även
- [HTTPbin för testning](http://httpbin.org)
- [Arduino WiFi Library Dokumentation](https://www.arduino.cc/en/Reference/WiFi)
