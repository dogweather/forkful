---
title:                "वेब पेज डाउनलोड करना"
html_title:           "Arduino: वेब पेज डाउनलोड करना"
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Kyun
"Tum karta hai web page download karna?" Ye sawal aksar Arduino programming ke beginners ke dimaag mein hota hai. Lekin jaane anjaane mein, web page download karna bahut important step hai jab hum IoT projects mein kaam karte hai. Isse hum internet se data ko retrieve kar sakte hai aur apne projects ko internet se connect kar sakte hai.

## Kaise Kare
Web page download karna bahut hi aasan hai Arduino programming language mein. Neeche diye gaye code blocks mein maine tumhe step-by-step guide di hai uss process ko follow karne ka.

```
#include <ESP8266WiFi.h>
#include <WiFiClient.h>
#include <ESP8266WebServer.h>

// Connect to WiFi network
const char* ssid = "YourWiFiNetworkName";
const char* password = "YourWiFiPassword";

// Create an object to store the webpage
ESP8266WebServer server(80);

void setup() {
  Serial.begin(115200);
  
  // Connect to WiFi
  WiFi.begin(ssid, password);
  Serial.println("Connecting to WiFi...");
  
  // Wait for connection
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
  }
  
  // If WiFi is connected, print out the local IP address
  Serial.print("Connected to WiFi network! IP address: ");
  Serial.println(WiFi.localIP());
  
  // Load webpage
  server.on("/", handleRoot);
  server.begin();
  Serial.println("Web page loaded!");
}

void loop(){
  server.handleClient(); // Handles incoming client requests
}

// Function to handle root webpage
void handleRoot() {
  server.send(200, "text/html",
  "<html>\
    <head>\
      <title>My First Web Page</title>\
    </head>\
    <body>\
      <h1>Welcome to my first web page!</h1>\
      <p>This is a simple webpage created using Arduino.</p>\
    </body>\
  </html>");
}
```
Taaki yeh code sahi se kaam kare, tumhe kuch libraries install karni hogi apne Arduino IDE mein. Install karne ke liye, "Tools" menu se "Libraries Manager" par click karo aur "ESP8266WiFi, WiFiClient, ESP8266WebServer" libraries ko install kar lo.

Ab apne WiFi ke naam aur password ko `ssid` aur `password` variables mein dalke code ko upload kar do. Yeh code sabse pehle apne WiFi network mein connect karega aur phir `handleRoot()` function se webpage ko retrieve karega aur use print karega console par.

## Deep Dive
Web page download karna ek important step hai jab hum internet se data retrieve karna chahte hai ya apne projects ko internet se connect karte hai. Iske alawa, hum isse web-based user interface bhi create kar sakte hai jisse hum apne projects ko remotely control kar sakte hai.

Yeh process bilkul bhi complicated nahi hai. Bas apko libraries ko install karna hai aur apne WiFi network se connect hona hai. Yeh process sabse zyada popular hai jab hum IoT projects ke saath kaam karte hai.

## Dekho Bhi
- [ESP8266WiFi library documentation](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
- [WiFiClient library documentation](https://www.arduino.cc/en/Reference/WiFiClient)
- [ESP8266WebServer library documentation](https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266WebServer)