---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a webpage is the process of retrieving and storing the content of a webpage from the internet. Programmers do this to access and manipulate web data for a variety of purposes such as web scraping, testing, or offline browsing.

## How To:

Downloading a web page with Arduino requires Ethernet access, i.e., an Ethernet shield or module. Here's a simple piece of code in Arduino for this task:

```Arduino
#include <Ethernet.h>
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };

IPAddress ip(192,168,1,177);
EthernetClient client;
char server[] = "www.example.com";

void setup() {
  Serial.begin(9600);

  if (Ethernet.begin(mac) == 0) {
    Serial.println("Failed to configure Ethernet.");
    return;
  }
  delay(1000);
  
  if (client.connect(server, 80)) {
    Serial.println("connected");
    client.println("GET / HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
  } else {
    Serial.println("connection failed");
  }
}

void loop() {
  if (client.available()) {
    char c = client.read();
    Serial.print(c);
  }
  if (!client.connected()) {
    Serial.println();
    Serial.println("disconnecting.");
    client.stop();
    while(true);
  }
}
```

This script connects to a server (www.example.com), sends an HTTP GET request, and then simply prints the server's response to the Serial Monitor.

## Deep Dive

Historically, downloading and saving a webpage was a sophisticated task. However, with devices like Arduino and associated libraries, webpages can be accessed with just a short sketch of code.

An alternative to the approach above might involve using a Wi-Fi module/shield if Ethernet isn't available, or perhaps utilizing another programming language or platform more suited to web-oriented tasks, such as Python or Javascript.

Under the hood, the process of downloading a webpage starts with establishing a TCP/IP connection to the server via the HTTP protocol (or its secure variant HTTPS). The Arduino sends an HTTP GET request, to which the server responds with the HTML content of the webpage. This content is then read and stored by the Arduino.

## See Also

For further diving, check out:
- Arduino Ethernet Shield info: https://www.arduino.cc/en/Main/ArduinoEthernetShield
- Arduino Ethernet Library: https://www.arduino.cc/en/reference/ethernet
- More info on HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview