---
title:                "Arduino recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

In today's digital age, the internet has become an essential part of our daily lives. From social media to online shopping, we rely on the internet for a multitude of tasks. As technology continues to advance, it has become easier to access the internet through various devices such as smartphones, laptops, and even microcontrollers like Arduino. With the ability to connect to the internet, comes the opportunity to download web pages directly onto an Arduino board, opening up a whole new world of possibilities for makers and developers.

## How To

To download a web page onto an Arduino board, you will need to use a combination of Arduino's WiFi libraries and an HTTP client library such as the widely used "ESP8266WiFi.h". The code will differ depending on the WiFi module you are using, but the basic steps are similar. Firstly, you will need to establish a connection to the internet by providing your WiFi credentials. Once connected, you can use the HTTP client library to make an HTTP GET request and fetch the webpage's source code. This code can then be stored in variables and manipulated as needed. Here's an example of how to download the homepage of Google:

```
#include <ESP8266WiFi.h>
WiFiClient client;

void setup() {
  Serial.begin(115200);

  // Connect to WiFi network
  WiFi.begin("SSID", "password");

  // Wait for connection
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
  }

  Serial.println("Connected to WiFi!");

  // Make HTTP GET request to Google
  if (client.connect("www.google.com", 80)) {
    client.println("GET / HTTP/1.1");
    client.println("Host: www.google.com");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  // Print response from server
  while (client.available()) {
    char c = client.read();
    Serial.print(c);
  }

  // Disconnect from server
  if (!client.connected()) {
    Serial.println();
    Serial.println("Connection closed.");
    client.stop();
    
    // Do something with the downloaded data
    // e.g. store in variables, display on LCD screen, etc.
  }
}
```

## Deep Dive

Behind the scenes, the Arduino board is acting as a client and communicating with a server (in this case, Google) using the Hypertext Transfer Protocol (HTTP). HTTP is a standardized protocol for transferring data over the internet and is the foundation of the World Wide Web. When we make an HTTP GET request, we are asking the server to send us a specific resource (in this case, the homepage of Google). The server responds with a status code and the source code of the webpage. We can then parse and manipulate this code to suit our needs.

There are other HTTP methods such as POST, PUT, and DELETE that can also be used to interact with web pages through Arduino. Additionally, there are other HTTP client libraries that offer different functionalities and are worth exploring for more complex projects.

## See Also

- [ESP8266WiFi Library](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
- [HTTP Client Library for Arduino](https://github.com/arduino-libraries/ArduinoHttpClient)
- [HTTP Tutorial for Arduino](https://randomnerdtutorials.com/esp32-esp8266-http-get-post-arduino/)