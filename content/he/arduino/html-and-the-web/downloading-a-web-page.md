---
date: 2024-01-20 17:43:51.451728-07:00
description: "How to (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D0\u05EA\
  \ \u05D6\u05D4) \u05E7\u05D5\u05D3 \u05DC\u05D3\u05D5\u05D2\u05DE\u05D0."
lastmod: '2024-04-05T22:37:48.237375-06:00'
model: gpt-4-1106-preview
summary: "How to (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D0\u05EA \u05D6\
  \u05D4) \u05E7\u05D5\u05D3 \u05DC\u05D3\u05D5\u05D2\u05DE\u05D0."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
weight: 42
---

## How to (איך לעשות את זה)
קוד לדוגמא:
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid     = "YOUR_SSID";
const char* password = "YOUR_PASSWORD";

void setup() {
   Serial.begin(115200);
   WiFi.begin(ssid, password);

   while (WiFi.status() != WL_CONNECTED) {
     delay(1000);
     Serial.println("Connecting to WiFi...");
   }

   Serial.println("Connected to WiFi");
}

void loop() {
  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http;
    http.begin("http://example.com"); // URL to download
    int httpCode = http.GET();

    if (httpCode > 0) {
      String payload = http.getString();
      Serial.println(payload);
    } else {
      Serial.println("Error on HTTP request");
    }
    http.end();
  }
  delay(10000); // Wait for 10 seconds
}
```
פלט לדוגמה:
```
Connecting to WiFi...
Connected to WiFi
<!doctype html>...
```

## Deep Dive (צלילה לעומק)
להורדת דף אינטרנט דרך Arduino, פעם ראשונה עשו זאת בעזרת מודולים כמו ה ESP8266. אפשרויות חלופיות כוללות שימוש ב-ESP32 או מיקרו-בקרים אחרים עם יכולות WiFi. ההבנה איך HTTP עובד חשובה לביצוע בקשות יעילות והבנת תגובות מהשרת.

## See Also (ראה גם)
- [ESP8266 NodeMCU HTTP GET and HTTP POST with Arduino IDE](https://randomnerdtutorials.com/esp8266-nodemcu-http-get-post-arduino/)
- [ESP8266WiFi library documentation](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
- [HTTP Client library for the ESP8266](https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266HTTPClient)
