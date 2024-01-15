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

## Why

You may be wondering why someone would want to send an HTTP request using their Arduino board. Well, the answer is simple - to access and retrieve data from the internet! By sending an HTTP request, you can communicate with web servers and retrieve information that can enhance your projects, such as weather data or real-time stock prices.

## How To

To send an HTTP request using Arduino, you'll need an Ethernet or WiFi shield to connect your board to the internet. Once you have that set up, here's a simple code example using the built-in WiFi library:

```
#include <WiFi.h> //include WiFi library
char ssid[] = "Your Network Name"; //replace with your network name
char password[] = "Your Network Password"; //replace with your network password
int status = WL_IDLE_STATUS; //wifi connection status
WiFiClient client;

void setup() {
  Serial.begin(9600); //initialize serial communication
  while (!Serial) {} //wait for serial port to connect
  //attempt to connect to WiFi network
  while (status != WL_CONNECTED) {
    Serial.print("Attempting to connect to Network named: ");
    Serial.println(ssid);
    status = WiFi.begin(ssid, password);
  }
  Serial.println("Connected to Wifi!"); //print confirmation message
}

void loop() {
  //establish connection with server
  if (client.connect("example.com", 80)) {
    Serial.println("Successfully connected to server!");
    //send HTTP request
    client.println("GET / HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();
  }
  else {
    Serial.println("Connection failed."); //print error message
  }
}
```

This code connects your board to your WiFi network and sends a basic HTTP GET request to the server "example.com". Make sure to replace the network name and password with your own, as well as edit the server in the code to the one you want to communicate with.

Upon running this code, you should see the serial monitor print out a successful connection message. You can also add another `Serial.println()` statement to print out the response from the server, which can be useful for troubleshooting.

## Deep Dive

Now, let's break down the `client.println()` statements in the code. The first line, `GET / HTTP/1.1`, is the actual HTTP request. The "GET" method is used to retrieve data from the server, and the "/" after it is the path or resource you want to access. In this case, we are accessing the root directory of the server. The "HTTP/1.1" indicates the HTTP version being used.

The next line, `Host: example.com`, tells the server which host or domain you want to access. This is necessary for servers that host multiple websites or have different IP addresses.

The third line, `Connection: close`, specifies the type of connection to be used. In this case, we are closing the connection after the request is made. Other options include "keep-alive", which keeps the connection open for multiple requests, and "upgrade", which requests a different protocol for the connection.

It's important to note that in order to receive a response from the server, you may need to add a `delay()` statement after sending the request. This gives the server time to process the request and send a response back.

## See Also

For more information on sending HTTP requests using Arduino, check out these resources:

- [Arduino WiFi library documentation](https://www.arduino.cc/en/Reference/WiFi)
- [HTTP GET request tutorial by Programming Electronics Academy](https://programmingelectronics.com/http-requests-with-arduino/)
- [ESP8266 HTTP GET tutorial by Random Nerd Tutorials](https://randomnerdtutorials.com/esp8266-http-get-request-getting-started/)