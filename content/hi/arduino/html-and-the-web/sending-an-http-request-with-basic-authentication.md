---
aliases:
- /hi/arduino/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:01:28.398551-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u090F\u0915 \u0924\u0930\u0940\u0915\u093E \u0939\u0948 \u091C\u093F\u0938\u0938\
  \u0947 \u0906\u092A\u0915\u093E Arduino \u0907\u0902\u091F\u0930\u0928\u0947\u091F\
  \ \u092A\u0930 \u092E\u094C\u091C\u0942\u0926 \u0938\u0930\u094D\u0935\u0930 \u0938\
  \u0947 \u092C\u093E\u0924 \u0915\u0930 \u0938\u0915\u0924\u093E \u0939\u0948\u0964\
  \ \u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\
  , \u0906\u092A \u0909\u0938\u0947 \u0938\u0941\u0930\u0915\u094D\u0937\u093F\u0924\
  \ \u0924\u0930\u0940\u0915\u0947 \u0938\u0947 \u0915\u0930\u2026"
lastmod: 2024-02-18 23:09:03.814579
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u090F\u0915 \u0924\u0930\u0940\u0915\u093E \u0939\u0948 \u091C\u093F\u0938\u0938\
  \u0947 \u0906\u092A\u0915\u093E Arduino \u0907\u0902\u091F\u0930\u0928\u0947\u091F\
  \ \u092A\u0930 \u092E\u094C\u091C\u0942\u0926 \u0938\u0930\u094D\u0935\u0930 \u0938\
  \u0947 \u092C\u093E\u0924 \u0915\u0930 \u0938\u0915\u0924\u093E \u0939\u0948\u0964\
  \ \u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\
  , \u0906\u092A \u0909\u0938\u0947 \u0938\u0941\u0930\u0915\u094D\u0937\u093F\u0924\
  \ \u0924\u0930\u0940\u0915\u0947 \u0938\u0947 \u0915\u0930\u2026"
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध भेजना एक तरीका है जिससे आपका Arduino इंटरनेट पर मौजूद सर्वर से बात कर सकता है। बेसिक प्रमाणीकरण का उपयोग करके, आप उसे सुरक्षित तरीके से कर सकते हैं। प्रोग्रामर्स आमतौर पर डाटा भेजने या मांगने के लिए इस प्रक्रिया का इस्तेमाल करते हैं।

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
