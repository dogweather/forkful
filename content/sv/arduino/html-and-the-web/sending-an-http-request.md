---
date: 2024-01-20 17:59:04.638473-07:00
description: "HTTP-beg\xE4ran skickar data till server f\xF6r att h\xE4mta eller \xE4\
  ndra resurser. Programmerare skickar dessa f\xF6r att interagera med webbtj\xE4\
  nster och API:er."
lastmod: '2024-02-25T18:49:36.480021-07:00'
model: gpt-4-1106-preview
summary: "HTTP-beg\xE4ran skickar data till server f\xF6r att h\xE4mta eller \xE4\
  ndra resurser. Programmerare skickar dessa f\xF6r att interagera med webbtj\xE4\
  nster och API:er."
title: "Skicka en http-f\xF6rfr\xE5gan"
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
