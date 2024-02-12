---
title:                "Downloading a web page"
aliases: - /en/arduino/downloading-a-web-page.md
date:                  2024-01-20T17:43:19.940917-07:00
model:                 gpt-4-1106-preview
simple_title:         "Downloading a web page"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means fetching the HTML content from the URL you're looking at. Programmers do this to pull data, update their gadgets, or simply use the internet for more than cat videos.

## How to:

Here's the nitty-gritty: make your Arduino surf the web and grab what you need.

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

  HTTPClient http;
  http.begin("http://example.com"); // Swap with your URL
  
  int httpCode = http.GET();
  
  if (httpCode > 0) {
    if (httpCode == HTTP_CODE_OK) {
      String payload = http.getString();
      Serial.println(payload);
    }
  } else {
    Serial.printf("Error in HTTP request: %s\n", http.errorToString(httpCode).c_str());
  }
  http.end();
}

void loop() {
  // Nothing here for now.
}
```

Power it up, and you should see the webpage's HTML in the Serial Monitor. Remember, you'll need the ESP8266 Wi-Fi module and a connection.

## Deep Dive

Once upon a time, Arduinos were simple offline creatures. Then came shields and modules that connected them to the big bad web. ESP8266 is one such magical gizmo, turning your Arduino into an internet surfer.

Alternatives? You bet. There's the ESP32, Ethernet Shield, and others for the same job.

Quality of your internet connection, power supply robustness, and even the time of day could tip the scales on how well your Arduino downloads that page. We're really plugging into more factors than just writing slick code.

## See Also

Dig more? Check these out:

- [Arduino Networking](https://www.arduino.cc/en/Guide/ArduinoEthernetShield)
- [ESP8266 GitHub Wiki](https://github.com/esp8266/Arduino)
- [ESP32 GitHub Repo](https://github.com/espressif/arduino-esp32)
