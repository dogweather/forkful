---
date: 2024-01-20 18:01:28.398551-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: ."
lastmod: '2024-03-13T22:44:52.774577-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

## कैसे करें:
```Arduino
#include <ESP8266WiFi.h>
#include <Base64.h>

const char* ssid = "आपका_WiFi_नाम";
const char* password = "आपका_WiFi_पासवर्ड";
const char* server = "example.com";
const char* apiEndpoint = "/api/data"; // API एंडपॉइंट
const char* user = "user";  // बेसिक प्रमाणीकरण यूजरनेम
const char* pass = "pass";  // बेसिक प्रमाणीकरण पासवर्ड

WiFiClient client;

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);
  
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  if (client.connect(server, 80)) {
    String auth = user + ":" + pass;
    String encodedAuth = base64::encode(auth);
  
    // HTTP अनुरोध बनाना
    String httpRequest = String("GET ") + apiEndpoint + " HTTP/1.1\r\n" +
                         "Host: " + server + "\r\n" +
                         "Authorization: Basic " + encodedAuth + "\r\n\r\n";
    client.print(httpRequest);
  }
}

void loop() {
  if (client.available()) {
    String line = client.readStringUntil('\r');
    Serial.println(line);
  }
}
```
सैंपल आउटपुट:
```
HTTP/1.1 200 OK
Content-Type: application/json
{"status":"success","message":"Hello, world!"}
```

## डीप डाइव:
HTTP बेसिक प्रमाणीकरण एक सरल लेकिन प्राचीन तकनीक है, जिसे वेब की शुरुआती दिनों से इस्तेमाल किया जा रहा है। यह यूजरनेम और पासवर्ड को बेस64 एन्कोडिंग में परिवर्तित करके एचटीटीपी हेडर्स में भेजता है। एचटीटीपीएस जैसे अधिक सुरक्षित विकल्प भी हैं। इसके अलावा, मॉडर्न अप्रोचेस जैसे कि OAuth का इस्तेमाल करके ज्यादा सुरक्षित और नियंत्रण योग्य प्रमाणीकरण संभव है। Arduino में, यह प्रक्रिया ESP8266WiFi जैसे लाइब्रेरीज का उपयोग करके सरलीकृत है।

## देखें भी:
- Arduino के लिए ESP8266WiFi लाइब्रेरी डॉक्युमेंटेशन: https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html
- बेसिक प्रमाणीकरण के लिए RFC 7617: https://tools.ietf.org/html/rfc7617
- Arduino और REST API इंटिग्रेशन गाइड: https://www.arduino.cc/en/Guide/ArduinoEthernetShield
- Base64 एन्कोडिंग/डिकोडिंग लाइब्रेरी: https://github.com/adamvr/arduino-base64
