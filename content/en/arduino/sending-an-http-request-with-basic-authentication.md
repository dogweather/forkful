---
title:                "Sending an http request with basic authentication"
date:                  2024-01-20T18:00:50.041796-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an http request with basic authentication"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request with basic authentication adds a layer of security by requiring a username and password. Programmers use it to access APIs or web services that are locked down to authorized users only.

## How to:
To make this happen on an Arduino, you first need to include the necessary libraries – typically `<ESP8266WiFi.h>` for ESP8266 or `<WiFi.h>` for ESP32, and `<Base64.h>` for encoding authentication details. Here's a bare-bones snippet to get you started:

```Arduino
#include <ESP8266WiFi.h>
#include <Base64.h>

const char* ssid = "yourSSID";
const char* password = "yourPassword";
const char* server = "your.server.com";
const char* authUser = "user";
const char* authPass = "pass";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  
  String auth = "Basic " + base64::encode(String(authUser) + ":" + String(authPass));

  WiFiClient client;
  if (client.connect(server, 80)) {
    client.println("GET /route HTTP/1.1");
    client.print("Host: ");
    client.println(server);
    client.println("Authorization: " + auth);
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  // Your regular code here
}
```

Upon running, the Arduino will connect to the specified server with the credentials and fetch the protected content.

## Deep Dive

HTTP Basic Authentication has been around since the early days of the web, defined in 1996 by RFC 2617. It's simple: encode username and password in base64 and slap it onto an HTTP header. It's not the most secure method (because base64 is easily reversible), but it's straightforward for low-stakes or internal tools.

There are alternatives, like Digest Access Authentication or OAuth, which are more secure, but they're also heavier on resources – something to consider on a tiny Arduino.

For implementation, keep in mind that base64 encoding increases the size of the creds by about 33%, and Arduino's memory is limited. Also, ensure your server uses SSL/TLS (HTTPS) if you're sending creds over the internet to avoid exposure.

## See Also
- [Wikipedia on Basic access authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Secure your HTTP request](https://arduino.cc/en/Tutorial/WebClientRepeating)