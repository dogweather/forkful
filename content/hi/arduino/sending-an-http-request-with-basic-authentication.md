---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध भेजना बुनियादी प्रमाणीकरण के साथ, एक वेब सर्वर से डेटा प्राप्त करने का एक तकनीक है, जिसमें यूज़रनेम और पासवर्ड की मांग की जाती है। प्रोग्रामर्स इसे उपयोग करते हैं क्योंकि यह उन्हें सुरक्षित ढंग से वेब सर्वर के साथ बातचीत करने की अनुमति देता है। 

## कैसे करें:

Arduino के साथ कोड लिखने का एक उदाहरण नीचे दिया गया है:

```Arduino
#include <ESP8266HTTPClient.h>

void setup() {
  Serial.begin(115200); 
}

void loop() {
  HTTPClient http;  
  http.begin("http://example.com"); 
  http.setAuthorization("username", "password"); 
  int httpCode = http.GET();  
  if(httpCode > 0) {
     String payload = http.getString(); 
     Serial.println(payload); 
  }
  http.end();
}
```

## गहरी तक 

बुनियादी प्रमाणीकरण के साथ HTTP अनुरोध भेजने की प्रक्रिया वर्षों से अभ्यास में है, लेकिन इसमें मुख्य जोखिम यह है कि यदि कनेक्शन असुरक्षित है, तो कोई अनाधिकृत उपयोगकर्ता आपके क्रेडेंशियल्स को इंटरसेप्ट कर सकता है। 

इसके बदले में, आप OAuth जैसे प्रमाणीकरण मेकेनिज्म का उपयोग कर सकते हैं, यदि आपके पास उच्च स्तर की सुरक्षा की आवश्यकता है। 

आप HTTPClient लाइब्रेरी का उपयोग करके Arduino में HTTP अनुरोध भेज सकते हैं। 'http.begin()' कला में, आप यूआरएल पास करते हैं जहां आप HTTP अनुरोध भेजना चाहते हैं। 'http.setAuthorization()' हमें यूज़रनेम और पासवर्ड प्रदान करने की अनुमति दे रहा है। और 'http.GET()' HTTP अनुरोध भेजता है। 

## और देखें 

1. Official Arduino documentation: https://www.arduino.cc/reference/en/
2. Guide to HTTPClient library: https://arduino-esp8266.readthedocs.io/en/latest/httpclient.html
3. More on HTTP Basic Access Authentication: https://tools.ietf.org/html/rfc7617