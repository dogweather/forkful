---
title:                "Arduino recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why
Sending an HTTP request with basic authentication is a common practice in the world of Arduino programming. It allows users to securely transmit information over the internet, which is essential for many projects such as IoT devices or web-enabled sensors. 

## How To 
To send an HTTP request with basic authentication in Arduino, follow these steps: 
1. First, import the necessary libraries for the project. This may include the `WiFiClientSecure` and `Base64` libraries. 
2. Define the necessary variables, such as server, port number, and credentials. 
3. Create a `WiFiClientSecure` object and connect it to the server. 
4. Use the `setAuthorization` function to add the basic authentication header with the credentials. 
5. Use the `print` function to write the HTTP request method, path, and any necessary headers. 
6. Finally, send the request using the `flush` and `readString` functions to get the response from the server. 

Here is an example of how the code may look like in Arduino: 

```
// Import libraries
#include <WiFiClientSecure.h>
#include <Base64.h>

// Define necessary variables
const char* server = "www.example.com";
int port = 443;
const char* username = "user123";
const char* password = "pass456";

// Create WiFiClientSecure object and connect to server
WiFiClientSecure client;
if (!client.connect(server, port)) {
    Serial.println("Connection failed!");
}

// Add basic authentication header
String auth = String(username) + ":" + String(password);
String authHeader = "Authorization: Basic " + base64::encode(auth);
client.setAuthorization(authHeader.c_str());

// Write HTTP request and send it
client.print("GET /data HTTP/1.1\r\n");
client.print("Host: www.example.com\r\n");
client.println("Connection: close\r\n");
client.flush();

// Read and print response from server
while (client.available()) {
    String line = client.readStringUntil('\r');
    Serial.print(line);
}

```

Here is a sample output from the code above: 

```
HTTP/1.1 200 OK
Date: Thu, 01 Jul 2021 01:23:45 GMT
Content-Type: application/json
Content-Length: 243

{"temperature": 25.4, "humidity": 60, "pressure": 1012}
```

## Deep Dive
Sending an HTTP request with basic authentication requires the use of a base64 encoding library, as the credentials need to be encoded before being sent. Base64 is a commonly used encoding scheme that converts binary data into ASCII characters, allowing it to be transmitted through text-based protocols such as HTTP.

In the code example above, we used the `Base64.h` library to encode the credentials. This library provides the `encode` function which takes in a string and returns the base64 encoded version of the string.

It is important to note that basic authentication is not the most secure method of authentication as the credentials are sent in plain text. However, it provides a simple and quick solution for authentication in Arduino projects.

## See Also
- [HTTP Client Library for Arduino](https://github.com/arduino-libraries/ArduinoHttpClient)
- [Base64 Arduino Library](https://github.com/espressif/arduino-esp32/tree/master/libraries/Arduino_Base64)
- [HTTP Basic Authentication on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)