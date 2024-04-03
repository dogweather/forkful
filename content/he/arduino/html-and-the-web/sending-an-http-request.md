---
date: 2024-01-20 17:59:32.313695-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D4\u05D9\
  \u05D0 \u05D3\u05E8\u05DA \u05DC\u05EA\u05E7\u05E9\u05E8 \u05E2\u05DD \u05E9\u05E8\
  \u05EA\u05D9\u05DD \u05D1\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8 \u05D9\u05E9\
  \u05D9\u05E8\u05D5\u05EA \u05DE\u05D4-Arduino. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05E9\u05DC\u05D5\u05D7 \u05D0\u05D5 \u05DC\u05E7\u05D1\u05DC \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD, \u05DC\u05D2\u05E9\u05EA \u05DC-APIs \u05D0\u05D5 \u05DC\
  \u05E9\u05DC\u05D5\u05D8 \u05D1\u05DE\u05DB\u05E9\u05D9\u05E8\u05D9\u05DD \u05DE\
  \u05E8\u05D7\u05D5\u05E7."
lastmod: '2024-03-13T22:44:39.762535-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D4\u05D9\
  \u05D0 \u05D3\u05E8\u05DA \u05DC\u05EA\u05E7\u05E9\u05E8 \u05E2\u05DD \u05E9\u05E8\
  \u05EA\u05D9\u05DD \u05D1\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8 \u05D9\u05E9\
  \u05D9\u05E8\u05D5\u05EA \u05DE\u05D4-Arduino."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
weight: 44
---

## What & Why? (מה ולמה?)
שליחת בקשת HTTP היא דרך לתקשר עם שרתים באינטרנט ישירות מה-Arduino. תכניתנים עושים זאת כדי לשלוח או לקבל נתונים, לגשת ל-APIs או לשלוט במכשירים מרחוק.

## How to: (איך לעשות:)
הנה קוד פשוט לשליחת בקשת GET:

```Arduino
#include <WiFi.h>

const char* ssid     = "yourSSID";
const char* password = "yourPASSWORD";
const char* serverName = "http://example.com/api";

WiFiClient client;

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Connecting to WiFi...");
  }
  Serial.println("Connected to WiFi");
  httpGETRequest(serverName);
}

void loop() {
  // Nothing here for this simple example
}

void httpGETRequest(const char* serverName) {
  client.setTimeout(5000);
  if (client.connect(serverName, 80)) {
    client.println("GET /api HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println(); // Important: end the header section with an empty line!

    while (client.connected()) {
      String line = client.readStringUntil('\n');
      if (line == "\r") {
        Serial.println("Headers received, reply:");
        break;
      }
    }
    // Read response
    String response = client.readStringUntil('\n');
    Serial.println(response);
  } else {
    Serial.println("Connection failed.");
  }
}
```

הקוד מחבר ל-WiFi ושולח בקשת GET. פלט הדוגמה:

```
Connecting to WiFi...
Connected to WiFi
Headers received, reply:
{"example":"response"}
```

## Deep Dive (נסיון עמוק):
בעבר, שליחת בקשות HTTP מ-Arduino הייתה מורכבת יותר בגלל מגבלות החומרה של הפלטפורמה. כיום, עם התפתחות הטכנולוגיה ומודולים כמו ESP8266 ו-ESP32, התקשורת הפכה לקלה ונגישה יותר. ישנן גם חלופות לבקשות HTTP כגון MQTT, שמתאים לאינטרנט של הדברים (IoT). לבקשת HTTP יתרונות בזכות תקן אינטרנט רחב ותמיכה רחבה ב-APIs.

## See Also (ראה גם):
- דוקומנטציה של קליינט WiFi ל-ESP8266 ו-ESP32: [ESP8266WiFi library](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html), [ESP32 WiFi library](https://docs.espressif.com/projects/arduino-esp32/en/latest/esp-idf/api-reference/network/esp_wifi.html)
- לקרוא על פרוטוקול MQTT: [MQTT.org](http://mqtt.org/)
- איך להשתמש ב-Arduino עם שירותי ענן IoT: [Arduino Cloud](https://create.arduino.cc/cloud)
