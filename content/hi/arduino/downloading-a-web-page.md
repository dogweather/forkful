---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक वेब पेज डाउनलोड करना इसकी कंटेंट्स को अपने डिवाइस में स्थायी रूप से स्टोर करना होता है। प्रोग्रामर्स इसे डाटा ऐनालिटिक्स, वेब स्क्रेपिंग या ऑफलाइन उपयोग के लिए करते हैं।

## कैसे:

Arduino कोड़ की मदद से वेब पेज डाउनलोडिंग:

```Arduino 
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "your_SSID";
const char* password = "your_PASSWORD";

void setup() {

  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {

    delay(1000);
    Serial.println("Connecting...");
  }
}

void loop() {

  if (WiFi.status() == WL_CONNECTED) {

    HTTPClient http;
    http.begin("http://your_web_page.com");
    int httpCode = http.GET();

    if (httpCode > 0) {
      String payload = http.getString();
      Serial.println(payload);
    }
  http.end();
  }

  delay(3000);
}
```
इस कोड को चलाने से हम अपने Arduino पर वेब पेज की सामग्री प्राप्त करते हैं।

## गहरी डाइव:

ऐतिहासिक प्रक्षेपण में, वेब पेज डाउनलोड करने की आवश्यकता तभी प्रकट हुई जब इंटरनेट का विस्तार हुआ। इसके विकल्प में वेब सर्वर से डाटा प्राप्त करने के लिए अन्य प्रोटोकॉल भी मौजूद हैं, जैसे कि FTP, SNMP, आदि। वेब पेज डाउनलोडिंग आमतौर पर HTTP या HTTPS प्रोटोकॉल का उपयोग करके की जाती है।

## देखने के लिए भी:

1. [Arduino और वेब स्क्रेपिंग](https://www.arduino.cc/en/Tutorial/WebClient)
2. [ESP8266WiFi और ESP8266HTTPClient लाइब्रेरी](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
3. [Arduino पर माइक्रो वेब सर्वर विकसित करना](https://randomnerdtutorials.com/esp8266-web-server-with-arduino-ide/)