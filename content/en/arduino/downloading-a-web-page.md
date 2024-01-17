---
title:                "Downloading a web page"
html_title:           "Arduino recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page is the process of retrieving data from a website and displaying it on a device. Programmers do this to access information or resources from the internet easily and efficiently.

## How to:

```
ArduinoClient client;                        // create a client object
client.connect("www.example.com", 80);       // connect to website
Serial.println(client.getResponse());        // print website response
```

Sample output:

```
HTTP/1.1 200 OK                            // indicates successful connection
Date: Fri, 21 May 2021 12:00:00 GMT        
Content-Type: text/html; charset=UTF-8     // specifies content type
Connection: close                          // closes connection after response
...                                        // additional website data follows
```

## Deep Dive:

Downloading a web page has become an essential function for many devices, including Arduino boards. In the past, it was mostly used for retrieving simple text-based web pages. However, with the advancements in technology and the widespread use of the internet, downloading web pages has evolved to include multimedia content such as images, videos, and interactive elements.

There are various methods for downloading web pages, including HTTP, HTTPS, and WebSocket protocols. The example code above uses the HTTP protocol, which is the most common and simplest to implement. However, it is worth noting that HTTPS is a more secure option, as it uses encryption to protect data during transmission.

There are also alternatives to using the client object in Arduino, such as using other libraries or using an external module, like the ESP8266, to handle web requests. These options may be more suitable for more complex web pages that require authentication or large amounts of data.

Implementation details for downloading a web page may vary depending on the specific device and protocol being used. However, the general steps involve establishing a connection to the website, sending a HTTP request, and receiving a response. This response usually includes the webpage's HTML code, which can then be parsed and displayed on the device.

## See Also:

- [ESP8266 AT Command List](https://www.espressif.com/sites/default/files/documentation/4b-esp8266_at_instruction_set_en.pdf)
- [HTTP vs HTTPS: What's the Difference?](https://www.cloudflare.com/learning/ssl/why-is-http-not-secure/)
- [ArduinoHTTPClient Library](https://www.arduino.cc/en/Reference/HTTPClient)