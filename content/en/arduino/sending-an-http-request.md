---
title:                "Sending an http request"
html_title:           "Arduino recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request means establishing communication between a client (like your computer) and a server (like a website). Programmers do it to retrieve data from a server, such as a web page or an API response.

## How To:

To send an HTTP request using Arduino, you'll need the Ethernet library. Below is a basic example using the GET method:
```
#include <Ethernet.h>

byte server[] = { 192,168,1,1 }; // server IP address
char page[] = "/samplepage.html"; // page to request

EthernetClient client;

if (client.connect(server, 80)) { // connect to server on port 80
  client.print("GET ");
  client.print(page);
  client.println(" HTTP/1.1");
  client.println("Host: 192.168.1.1");
  client.println("Connection: close"); // close connection after response
  client.println(); // end of request
}
```

The output will be the response from the server, which you can read and parse in your code.

## Deep Dive:

HTTP (Hypertext Transfer Protocol) was created in 1989 by Tim Berners-Lee for exchanging information on the World Wide Web. It is now widely used for communication between clients and servers. There are several HTTP methods, such as GET, POST, PUT, and DELETE, that specify the type of request being made.

Apart from using the Ethernet library, you can also send HTTP requests using a WiFi shield or module. Additionally, you can use libraries like HttpClient or ESP8266HTTPClient for simplified implementation.

When sending an HTTP request, you must specify the correct header information, including the method, URL, and host. You can also add any necessary headers, such as authentication or content-type.

## See Also:

- Official Ethernet Library documentation: https://www.arduino.cc/en/Reference/Ethernet
- Alternatives to sending HTTP requests on Arduino: https://www.arduino.cc/en/Reference/WiFi101
- Arduino HttpClient library: https://github.com/amcewen/HttpClient
- ESP8266HTTPClient library: https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266HTTPClient