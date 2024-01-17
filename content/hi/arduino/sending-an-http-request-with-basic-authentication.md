---
title:                "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
html_title:           "Arduino: बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक HTTP अनुरोध भेजना बेसिक प्रमाणीकरण के साथ क्या है और क्यों प्रोग्रामर्स इसे करते हैं? बेसिक प्रमाणीकरण एक वेब सुरक्षा प्रोटोकॉल है जो सुरक्षित डेटा स्थानांतरण को सुनिश्चित करता है। प्रोग्रामर्स इसका उपयोग डेवलपमेंट और डेटा एक्सचेंज के दौरान करते हैं।

## कैसे करें:
अर्डुइनो बोर्ड के साथ HTTP अनुरोध भेजने के लिए कुछ सरल उदाहरण।

```
#include <WiFi.h>
#include <WiFiClientSecure.h>

const char *ssid = "YOUR_WIFI_NETWORK_NAME";
const char *password = "YOUR_WIFI_PASSWORD";
const char *host = "HOST_NAME";
const char *path = "/API_ENDPOINT";

void setup() {
  Serial.begin(9600);
  WiFi.begin(ssid, password);

  while(WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("");
  Serial.println("WiFi connected.");
  Serial.println("IP address: ");
  Serial.println(WiFi.localIP());

  WiFiClientSecure client;
  Serial.print("connecting to ");
  Serial.println(host);
  if(!client.connect(host, 443)) {
    Serial.println("connection failed");
    return;
  }

  String url = path + "?PARAMETER=VALUE";
  Serial.print("Requesting URL: ");
  Serial.println(url);
  client.print(String("GET ") + url + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" +
               "Authorization: Basic YOUR_AUTHORIZATION_KEY\r\n" +
               "Connection: close\r\n\r\n");
  Serial.println("Request sent");
}

void loop() {
  if (Serial.available()) {
    Serial.write(client.read());
  }
}
```

## गहराई में जाएं:
एक HTTP अनुरोध भेजने के बारे में अधिक जानकारी। इसका उपयोग सुरक्षित डेटा स्थानांतरण को सुनिश्चित करने के लिए किया जाता है। यह एक साधारण वेब सुरक्षा प्रोटोकॉल है जो इंटरनेट द्वारा डेटा को सुरक्षित रूप से स्थानांतरित करता है। अल्टर्नेटिव के रूप में, एक अलग प्रोटोकॉल के साथ सुरक्षित डेटा स्थानांतरण किया जा सकता है। एपीआई का अनुरोध भेजने से पहले, आपको एक अधिकृत्त मंच से उपयोगकर्ता आईडी और पासवर्ड आवश्यक होते हैं।

## भी देखें:
अधिक संबंधित स्रोतों के लिए लिंक।
- अर्डुइनो HTTP क्लायंट बिल्डर: https://www.arduino.cc/en/Reference/HttpClient
- वेब सुरक्षा प्रोटोकॉल: https://en.wikipedia.org/wiki/HTTP#Security