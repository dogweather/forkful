---
date: 2024-01-20 17:59:18.773359-07:00
description: "Sending an HTTP request is the way your Arduino talks to the web, like\
  \ asking a server to send back some data. Programmers do it to let their Arduino\u2026"
lastmod: '2024-03-13T22:45:00.319254-06:00'
model: gpt-4-1106-preview
summary: "Sending an HTTP request is the way your Arduino talks to the web, like asking\
  \ a server to send back some data. Programmers do it to let their Arduino\u2026"
title: Sending an HTTP request
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is the way your Arduino talks to the web, like asking a server to send back some data. Programmers do it to let their Arduino interact with APIs, fetch web content, or communicate with other internet-based services.

## How to:

Working with the Arduino requires the `WiFiNINA` library for network features. Here's how to send a simple GET request:

```Arduino
#include <WiFiNINA.h>

char ssid[] = "yourNetworkName";       // your network SSID (name)
char pass[] = "yourNetworkPass";       // your network password
int status = WL_IDLE_STATUS;           // the WiFi radio's status
char server[] = "example.com";         // server you want to connect to

WiFiClient client;

void setup() {
  Serial.begin(9600);                  // start serial for debugging
  WiFi.begin(ssid, pass);              // start the WiFi connection
  while (status != WL_CONNECTED) {     // wait for the connection:
    status = WiFi.status();
    delay(1000);
  }
  Serial.print("Connected to ");
  Serial.println(ssid);
}

void loop() {
  if (client.connect(server, 80)) {    // if you get a connection, send the request:
    client.println("GET / HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();                   // end of the request
  } else {
    Serial.println("Connection failed"); // if you didn't get a connection to the server:
  }

  while (client.connected()) {         // while you're connected, read the data:
    if (client.available()) {
      char c = client.read();
      Serial.print(c);
    }
  }

  if (!client.connected()) {           // if the server's disconnected, stop the client:
    client.stop();
  }

  delay(10000);                        // wait ten seconds before trying again
}
```

Sample Output:
```
HTTP/1.1 200 OK
Date: Mon, 23 Jan 2023 12:36:47 GMT
Server: Apache/2.4.1 (Unix)
...
```

## Deep Dive

The concept of sending an HTTP request from a microcontroller wasn't always a thing. In the past, microcontrollers were more about sensors and physical world interaction. But with the advent of the IoT (Internet of Things), these devices started to need web connectivity. The Arduino can now use libraries like `WiFiNINA` to handle these connections robustly.

Alternatives to `WiFiNINA` exist depending on your hardware. For instance, the `Ethernet` library leverages wired connections, while `WiFi101` works with older WiFi shields.

On the implementation side, making an HTTP request seems simple, but the handshake, headers, and HTTP methods (GET, POST, etc.) are part of a strict protocol that allows devices to communicate over the web. The Arduino abstracts much of this complexity, but understanding the basics helps troubleshoot when things don't go smoothly.

## See Also

- Arduino `WiFiNINA` library docs: https://www.arduino.cc/en/Reference/WiFiNINA
- HTTP protocol primer: https://developer.mozilla.org/en-US/docs/Web/HTTP
- Arduino project hub for web-connected projects: https://create.arduino.cc/projecthub
