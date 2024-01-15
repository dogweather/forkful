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

## Why

The world of programming and technology is continuously evolving, and as a result, we are moving towards a more interconnected and internet-based society. Downloading a web page using Arduino allows us to expand our knowledge and skills in this area, as well as explore the possibilities of integrating Arduino with web development.

## How To

To start off, you will need to have an Arduino board and a computer with the Arduino software installed. You can download the latest version of the software from the official Arduino website. Once you have your hardware and software set up, follow these simple steps:

1. Connect your Arduino board to your computer using a USB cable.
2. Open the Arduino IDE and create a new sketch.
3. Import the required libraries by adding the following lines of code at the beginning of your sketch:

```Arduino
#include <SPI.h>
#include <WiFiNINA.h>
```

4. Next, you will need to connect your Arduino to a network. You can do this by adding the following code to your sketch:

```Arduino
char ssid[] = "YourNetworkName"; // Replace with your network name
char pass[] = "YourNetworkPassword"; // Replace with your network password

int status = WL_IDLE_STATUS;
WiFiClient client; 

void setup() {
  // Attempt to connect to WiFi network:
  while ( status != WL_CONNECTED) { 
    status = WiFi.begin(ssid, pass);
  }
}
```

5. Now you can go ahead and download a web page. Let's say you want to download the Google home page. You can do this by adding the following code to your sketch:

```Arduino
if (client.connect("www.google.com", 80)) { // Connect to the Google server on port 80
  Serial.println("Connected to server!");
  client.println("GET / HTTP/1.1"); // Send a HTTP GET request
  client.println("Host: www.google.com"); // Specify the host name
  client.println("Connection: close"); // Close the connection after receiving the response
  client.println(); // Print an empty line after the headers

  while (client.available()) { // Keep reading data until there is none left
    char c = client.read(); // Read each character
    Serial.print(c); // Print it to the Serial Monitor
  }
}
```

6. Upload the sketch to your Arduino board and open the Serial Monitor to see the downloaded web page in HTML format.

## Deep Dive

The process of downloading a web page using Arduino involves establishing a connection with a server using the WiFIClient function and sending a HTTP GET request. The server then responds with the requested web page, which can be read by using the client.read() function. This allows us to access the source code of the web page and manipulate it in different ways, such as extracting specific data or sending data to a microcontroller.

Although this is a basic example, it opens up a world of possibilities for integrating Arduino with web development and IoT projects. With the help of various libraries, Arduino can even be used to host a local web server and create interactive web applications.

## See Also

To learn more about using Arduino for web development, check out these resources:

- [Official Arduino Website](https://www.arduino.cc)
- [Arduino WiFiNINA library reference](https://www.arduino.cc/en/Reference/WiFiNINA)
- [Arduino Code Examples for WiFiNINA](https://www.arduino.cc/en/Tutorial/LibraryExamples/WiFiNINASimpleWebClient)
- [Arduino Web Server tutorial](https://randomnerdtutorials.com/esp32-web-server-arduino-ide/)