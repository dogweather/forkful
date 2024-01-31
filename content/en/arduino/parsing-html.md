---
title:                "Parsing HTML"
date:                  2024-01-20T15:29:57.901457-07:00
html_title:           "Bash recipe: Parsing HTML"
simple_title:         "Parsing HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML means sifting through HTML code to extract useful bits - like scraping a phone number off a contact page. Why do it? To automate data collection or interact with webpages from your Arduino project.

## How to:
Arduino isn't naturally web-savvy, but with external modules (like ESP8266), you can connect and grab web content. Here we'll pull HTML and search for a specific tag:

```Arduino
#include <ESP8266WiFi.h>
#include <WiFiClient.h>

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";

const char* host = "example.com";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  WiFiClient client;
  
  if (!client.connect(host, 80)) {
    Serial.println("Connection failed");
    return;
  }
  
  client.println("GET / HTTP/1.1");
  client.print("Host: ");
  client.println(host);
  client.println("Connection: close");
  client.println();

  while (client.connected() || client.available()) {
    if (client.available()) {
      String line = client.readStringUntil('\n');
      if (line.indexOf("<title>") >= 0) {
        int startIndex = line.indexOf("<title>") + 7;
        int endIndex = line.indexOf("</title>");
        String pageTitle = line.substring(startIndex, endIndex);
        Serial.println(pageTitle);
      }
    }
  }
}

void loop() {
  // We run the setup once and obtain the info we're looking for. No need to loop.
}
```

Sample Output:
```
Example Domain
```

## Deep Dive:
Historically, microcontrollers like Arduino weren't designed for complex tasks like HTML parsing. That all changed with network-capable modules and libraries enriching their capabilities.

The key to HTML parsing is string manipulation. You're looking for patterns. But remember, HTML can be messy. It's not like JSON with its reliable structure. This approach works for simple tasks but can fail if the HTML changes unexpectedly.

Alternatives? Sure. If you're serious about parsing, consider an Arduino-compatible microcontroller with more power or ones that can run Linux, which opens up tools like Python with libraries designed for web scraping.

Arduino's simplicity is both a boon and a chain here. You can implement basic parsing without a fuss, but if you need to handle complex HTML or massive amounts of data, you've outgrown your Uno.

## See Also:
- [ESP8266 GitHub repository](https://github.com/esp8266/Arduino)
- [Arduino HttpClient library](https://github.com/arduino-libraries/ArduinoHttpClient)
- [Web scraping with Python](https://realpython.com/python-web-scraping-practical-introduction/)
