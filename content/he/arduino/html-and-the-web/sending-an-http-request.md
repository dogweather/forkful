---
title:                "שליחת בקשת HTTP"
aliases: - /he/arduino/sending-an-http-request.md
date:                  2024-01-20T17:59:32.313695-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

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
