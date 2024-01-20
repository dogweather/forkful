---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

HTTP requests allow two machines to communicate via the internet. Using basic authentication when sending an HTTP request is a simple way to secure the data transmitted. 

## How to:

We can easily send an HTTP GET request with basic authentication using the ESP8266WiFi and ESP8266HTTPClient libraries on the Arduino. 

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "your_SSID";
const char* password = "your_PASSWORD";

void setup () {

  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {

    delay(1000);
    Serial.println("Connecting...");

  }
}

void loop() {

  if (WiFi.status() == WL_CONNECTED) { //Check WiFi connection status
 
    HTTPClient http;  //Declare an object of class HTTPClient

    http.begin("http://your-server.com");  //Specify request destination
    http.addHeader("Authorization", "Basic your_base64_encoded_credentials");  //Add Basic Authentication header

    int httpCode = http.GET();                                     

    if (httpCode > 0) { //Check for the returning code
 
        String payload = http.getString();
        Serial.println(payload);
 
      }

    http.end();   //Close connection
  
  }

  delay(30000);    //Send a request every 30 seconds

}
```
The above piece of code connects to a Wi-Fi network, then sends a GET request with Basic Authentication to a specified server. The response is printed on the Serial Monitor. 

## Deep Dive

Basic authentication was part of HTTP/1.0 (1996) for protecting web resources. It uses base64 encoding which is not encryption; hence, it's often used with SSL/TLS.

Alternatively, Digest Access Authentication and Bearer tokens are more secure yet more complex forms of HTTP authentication than Basic.

On implementation, the "Authorization" header's syntax is "Basic base64(username:password)". It sends username and password on each request leading to inefficient transmissions on weak networks.

## See Also

1. HTTP/1.1 specification ("rfc2616"): <https://tools.ietf.org/html/rfc2616>
2. Arduino ESP8266WiFi library <https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html>
3. Arduino ESP8266HTTPClient library <https://arduino-esp8266.readthedocs.io/en/latest/esp8266httpclient/readme.html>
4. Base64 encoding <https://en.wikipedia.org/wiki/Base64>
5. Digest Access Authentication <https://en.wikipedia.org/wiki/Digest_access_authentication>
6. Bearer tokens <https://tools.ietf.org/html/rfc6750>