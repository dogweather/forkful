---
date: 2024-01-20 17:58:49.102790-07:00
description: "How to: (Kuinka tehd\xE4:) ."
lastmod: '2024-04-05T21:53:58.397487-06:00'
model: gpt-4-1106-preview
summary: ''
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
weight: 44
---

## How to: (Kuinka tehdä:)
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "yourSSID";
const char* password = "yourPassword";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting to WiFi...");
  }

  HTTPClient http;
  http.begin("http://yourserver.com/data"); // Your API endpoint
  int httpCode = http.GET();
  
  if (httpCode > 0) { 
    String payload = http.getString();
    Serial.println(httpCode);
    Serial.println(payload);
  } else {
    Serial.println("Error on HTTP request");
  }

  http.end(); 
}

void loop() {
  // Nothing to do here
}
```

## Deep Dive (Sukellus syvemmälle)
HTTP-pyyntöjen lähettäminen on webin perustoimintoja, joka dates back to the early days of the internet. Alternatives include using WebSocket for continuous connections or MQTT for IoT applications. Implementation  needs a network-capable microcontroller like the ESP8266 or ESP32 and relies on the underlying TCP/IP stack. Libraries like ESP8266HTTPClient simplify the process.

## See Also (Katso myös)
- [Arduino WiFi Library](https://www.arduino.cc/en/Reference/WiFi)
- [HTTP Made Really Easy](http://www.jmarshall.com/easy/http/)
- [Understanding MQTT](https://www.hivemq.com/mqtt-essentials/)
