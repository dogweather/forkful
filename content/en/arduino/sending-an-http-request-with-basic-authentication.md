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

## Why
Sending HTTP requests with basic authentication is a useful feature in Arduino programming for communicating with web servers. It allows for secure data transfer and access control, making it an essential tool for IoT projects and web-connected devices.

## How To
To send an HTTP request with basic authentication in Arduino, we will use the `WiFiClientSecure` library. First, make sure the library is installed by navigating to "Tools" > "Manage Libraries" in the Arduino IDE. Then, follow the steps below to create a client, set up the request, and receive a response.

```
Arduino Code:

#include <WiFiClientSecure.h>

WiFiClientSecure client; // create client object
const char* host = "www.example.com"; // host URL

void setup(){
  Serial.begin(9600); // initialize serial communication
  // connect to WiFi
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Connecting to WiFi..");
  }
  if (client.connect(host, 443)) { // connect to server with HTTPS (port 443)
    // set up basic authentication - replace username and password with actual values
    client.println("GET /api/data HTTP/1.1");
    client.println("Host: " + String(host));
    client.println("Authorization: Basic " + String(base64::encode("username:password")));
    client.println("Connection: close"); // ensure connection is closed after request is sent
    client.println(); // end of request
  }
}

void loop() {
  while (client.available()) {
    // read response and print to serial monitor
    Serial.write(client.read());
  }
  delay(5000); // wait 5 seconds before sending another request
}
```

Sample Output:
```
HTTP/1.1 200 OK
Date: Sat, 10 Jul 2021 12:00:00 GMT
Server: Apache
Content-Length: 120
Connection: close
Content-Type: application/json

{"data": "Hello World", "status": "success"}
```

## Deep Dive
When sending an HTTP request with basic authentication, the process involves establishing a secure connection with the server and providing a username and password in the request header using Base64 encoding. It is important to ensure that the correct HTTP method (GET, POST, etc.) and the correct URL and port are specified in the request. Additionally, it is best practice to close the connection after receiving the response to avoid any potential security risks.

## See Also
- [WiFiClientSecure Library Documentation](https://www.arduino.cc/en/Reference/WiFiClientSecure)
- [Base64 Encoding in Arduino](https://arduinojson.org/v6/api/base64/)
- [HTTP Methods Overview](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)