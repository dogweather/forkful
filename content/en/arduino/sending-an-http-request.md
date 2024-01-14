---
title:                "Arduino recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why
Sending an HTTP request is a crucial aspect of Arduino programming as it allows you to communicate with web servers, retrieve data, and perform actions online. This opens up a whole world of possibilities for your projects, from creating IoT devices to integrating with web services.

## How To
To send an HTTP request using Arduino, you will need to use the built-in Ethernet or WiFi library. Follow these steps to get started:

1. First, make sure you have an Ethernet or WiFi connection set up on your Arduino board.
2. Next, include the necessary library in your code by adding `#include <WiFi.h>` or `#include <Ethernet.h>` at the top.
3. Set up the necessary variables and credentials for your connection, such as the SSID and password for WiFi, or the server IP and port for Ethernet.
4. Initialize the connection using the `begin()` function, passing in your credentials as parameters.
5. Once the connection is established, you can use the `client.print()` or `client.println()` functions to send your HTTP request. For example:

```Arduino
// Send a GET request to a URL
client.println("GET /api/data?key=12345 HTTP/1.1");

// Send a POST request with JSON data
client.println("POST /api/data HTTP/1.1");
client.println("Content-Type: application/json");
client.println("Host: www.example.com");
client.println("{\"temperature\":\"25.5\",\"humidity\":\"50\"}");
```

6. Finally, end the request with `client.println()` and close the connection using `client.stop()`.
7. You can then process the response from the server using the `client.read()` or `client.readString()` functions.

## Deep Dive
There are a few key things to keep in mind when sending HTTP requests with Arduino:

1. Make sure to add a `Host` header to your request, specifying the domain or IP address of the server you are sending the request to. This is necessary for the server to route the request correctly.
2. You can use the `GET` method to retrieve data from a server, and the `POST` method to send data to a server. Other common methods include `PUT`, `DELETE`, and `PATCH`.
3. If you are sending a `POST` request with JSON data, make sure to specify the `Content-Type` header as `application/json`.
4. It is important to follow the correct syntax for an HTTP request, including the HTTP version at the end.
5. You can also add additional headers to your request if needed, such as authentication tokens or cookies.

For more in-depth information on the HTTP protocol and how to construct different types of requests, refer to the official documentation or online resources.

## See Also
- [Official Arduino Documentation on HTTP Client Library](https://www.arduino.cc/en/Reference/HttpClient)
- [Tutorial on Using the WiFi and HTTP Client Library on Arduino](https://www.instructables.com/id/How-to-upload-data-to-thingspeak-using-ESP8266-WiF/)
- [Guide on Constructing HTTP Requests](https://www.digitalocean.com/community/tutorials/how-to-debug-the-http-protocol-with-telnet)