---
title:                "Sending an http request with basic authentication"
html_title:           "Arduino recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request with basic authentication is a way to securely communicate with a server by providing a username and password. This is commonly done by programmers when they need to access restricted resources or perform actions on a remote server.

## How to:
```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

// Set up WiFi connection
WiFi.begin("SSID", "password");
while (WiFi.status() != WL_CONNECTED) {
  delay(1000);
}

// Create HTTP client object
HTTPClient http;

// Set basic authentication credentials
http.setAuthorization("username", "password");

// Specify target URL
http.begin("http://www.example.com");

// Send GET request and store response in a String variable
String response = http.getString();

// Print response to serial monitor
Serial.println(response);

// Close connection
http.end();
```

## Deep Dive:
- Basic authentication was first introduced in HTTP 1.0 and has since been deprecated in favor of more secure methods such as OAuth.
- Alternatives to basic authentication include token-based authentication and single sign-on (SSO) systems.
- To send an HTTP request with basic authentication, the client must include a base64-encoded version of the username and password in the "Authorization" header of the request.

## See Also:
- [HTTPClient library reference](https://www.arduino.cc/reference/en/libraries/httpclient/)
- [HTTP basic authentication explained](https://www.digitalocean.com/community/tutorials/how-to-use-basic-authentication-with-php)
- [HTTP basic authentication vs token-based authentication](https://stackoverflow.com/questions/44024666/basic-authentication-vs-token-based-authentication-for-api/44025874#44025874)