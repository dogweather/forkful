---
title:                "Завантаження веб-сторінки"
date:                  2024-01-20T17:43:32.606114-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/downloading-a-web-page.md"
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
