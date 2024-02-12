---
title:                "Skicka en http-förfrågan"
aliases: - /sv/arduino/sending-an-http-request.md
date:                  2024-01-20T17:59:04.638473-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en http-förfrågan"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
HTTP-begäran skickar data till server för att hämta eller ändra resurser. Programmerare skickar dessa för att interagera med webbtjänster och API:er.

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
