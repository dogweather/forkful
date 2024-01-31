---
title:                "הורדת דף אינטרנט"
date:                  2024-01-20T17:43:51.451728-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
להוריד דף אינטרנט זה לקבל את התוכן שלו דרך הרשת. תכניתנים עושים את זה כדי לאסוף נתונים, לעקוב אחרי מצבים או כדי להשתמש במידע ביישומים שלהם.

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
