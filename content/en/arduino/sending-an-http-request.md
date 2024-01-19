---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is a fundamental part of web communication, essentially it's how a device 'talks' to another device over the internet. Programmers do it to fetch or submit data from/to a server, enabling abilities like reading sensor data online or controlling appliances remotely using IoT.

## How to:

Let's dive into how we do this using the Arduino HTTP Client library. Here's a simple code to send a GET request. Make sure your Arduino is connected to the internet.

```Arduino
#include <HttpClient.h>
#include <ArduinoHttpClient.h>

EthernetClient ethernet;
HttpClient client = HttpClient(ethernet, server, port);

void setup() {
   Ethernet.begin(mac, ip);  
   Serial.begin(9600);
}

void loop() {
   Serial.println("Making GET request");
   client.get("/api"); 

   int statusCode = client.responseStatusCode(); 
   String response = client.responseBody();
   Serial.print("Status code: ");
   Serial.println(statusCode);
   Serial.print("Response: ");
   Serial.println(response);
}
```

When you upload and run, you should see the HTTP status code and response printed in the Serial Monitor.

## Deep Dive

Sending HTTP requests dates back to early days of the web. It paved the way for web interactivity, reshaping how we use the internet. 

Alternatives include UDP if speed is important and TCP for reliable transmission, both are less suited than HTTP for web communication but have use cases.

When working with the Arduino HttpClient, understand that it's light-weight, suitable for microcontroller with limited resources. Under the hood, it opens a socket to the server, sends HTTP headers, and reads the response.

## See Also

For further reading and examples, check these links:

- Arduino HttpClient Library Docs: https://www.arduino.cc/en/Reference/ArduinoHttpClient
- Guide on HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP
- More on IoT: https://en.wikipedia.org/wiki/Internet_of_things