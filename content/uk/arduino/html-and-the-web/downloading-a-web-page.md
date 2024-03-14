---
date: 2024-01-20 17:43:32.606114-07:00
description: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F\
  \ \u0432\u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438 \u2014 \u0446\
  \u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u043E\u0442\u0440\u0438\u043C\u0430\
  \u043D\u043D\u044F \u0434\u0430\u043D\u0438\u0445 \u0437 \u0406\u043D\u0442\u0435\
  \u0440\u043D\u0435\u0442\u0443 \u043D\u0430 \u0432\u0430\u0448 \u043F\u0440\u0438\
  \u0441\u0442\u0440\u0456\u0439. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\
  \u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\
  \u043B\u044F \u0437\u0431\u043E\u0440\u0443 \u0434\u0430\u043D\u0438\u0445, \u0432\
  \u0456\u0434\u0434\u0430\u043B\u0435\u043D\u043E\u0433\u043E \u043A\u0435\u0440\u0443\
  \u0432\u0430\u043D\u043D\u044F \u0430\u0431\u043E\u2026"
lastmod: '2024-03-13T22:44:49.718004-06:00'
model: gpt-4-1106-preview
summary: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F\
  \ \u0432\u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438 \u2014 \u0446\
  \u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u043E\u0442\u0440\u0438\u043C\u0430\
  \u043D\u043D\u044F \u0434\u0430\u043D\u0438\u0445 \u0437 \u0406\u043D\u0442\u0435\
  \u0440\u043D\u0435\u0442\u0443 \u043D\u0430 \u0432\u0430\u0448 \u043F\u0440\u0438\
  \u0441\u0442\u0440\u0456\u0439. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\
  \u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\
  \u043B\u044F \u0437\u0431\u043E\u0440\u0443 \u0434\u0430\u043D\u0438\u0445, \u0432\
  \u0456\u0434\u0434\u0430\u043B\u0435\u043D\u043E\u0433\u043E \u043A\u0435\u0440\u0443\
  \u0432\u0430\u043D\u043D\u044F \u0430\u0431\u043E\u2026"
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Завантаження веб-сторінки — це процес отримання даних з Інтернету на ваш пристрій. Програмісти роблять це для збору даних, віддаленого керування або моніторингу контенту.

## How to: (Як зробити:)
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);
  
  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting to WiFi...");
  }

  Serial.println("Connected to the WiFi network");
  
  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http;
    http.begin("http://example.com"); //Replace with your URL
    int httpCode = http.GET();
    
    if (httpCode > 0) {
        Serial.printf("HTTP Code: %d\n", httpCode);
        String payload = http.getString();
        Serial.println("Received webpage:");
        Serial.println(payload);
    } else {
        Serial.printf("Error in HTTP request. HTTP Code: %d\n", httpCode);
    }
    http.end();
  }
}

void loop() {
}
```
Output:
```
Connecting to WiFi...
Connected to the WiFi network
HTTP Code: 200
Received webpage:
<!DOCTYPE html><html><head><title>Example Domain</title>...
```

## Deep Dive (Поглиблений Аналіз)
Back in the early 2000s, when microcontrollers started gaining internet access, downloading a webpage was a complex task. Now, with modules like the ESP8266, it's simpler. ESP8266 is a Wi-Fi-capable microchip allowing Arduino boards to access the internet. While the provided code uses ESP8266, alternatives like Ethernet Shield for wired connections or ESP32 for faster Wi-Fi exist. Implementation details include initiating Wi-Fi connection, creating an HTTP client, making a GET request, and processing the response. Remember, managing large strings and ensuring secure connections can be tricky on constrained devices like Arduinos.

## See Also (Дивіться Також)
For further reading and related references, check out:
- Arduino JSON library for parsing JSON data: [ArduinoJson](https://arduinojson.org/)
- Examples of web server implementation on Arduino: [Arduino Web Server](https://www.arduino.cc/en/Tutorial/LibraryExamples/WebServer)
