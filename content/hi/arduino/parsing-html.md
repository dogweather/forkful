---
title:                "Arduino: HTML को व्याख्यांसंचयन करना"
simple_title:         "HTML को व्याख्यांसंचयन करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों:

HTML पार्सिंग में लिप्त होने का मंगल निशाना है क्योंकि यह आपको विभिन्न डेटा स्रोतों से सुझाव प्रदान कर सकता है, जो आपको अपने आर्डुइनो प्रोजेक्ट्स में उपयोगी हो सकते हैं।

## कैसे करें:

HTML को पार्स करने के लिए आपको सबसे पहले HTML डेटा को डेटा स्ट्रक्चर में पढ़ना होगा। फिर, आप आवश्यक टैग को खोजने और उस से डेटा निकालने के लिए कोड को लिख सकते हैं। नीचे दिए गए उदाहरण में, हमें दिए गए HTML कोड और उससे प्राप्त डेटा को प्रिंट करने के लिए आर्डुइनो कोड के बारे में दिखाया गया है।

```
Arduino Code:

#include <SPI.h>
#include <WiFi101.h>
#include <WiFiUdp.h>

char html[] = "<html><head><title>Arduino प्रोग्रामिंग ब्लॉग पोस् ट</title></head><body><h1>हैलो हिन्दी पाठकों!</h1></body></html>"; //आप अपना खुद का HTML डेटा भी उपयोग कर सकते हैं

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; //जब तक सीरियल पोर्ट शुरू नहीं होता है, यह हंग हो सकता है।
  }
  
  if (WiFi.status() == WL_NO_SHIELD) {
    Serial.println("WiFi शील्ड से जुड़ने में समस्या हो सकती है।");
    while (true);
  }

  WiFi.begin("SSID", "password"); //अपना वाईफाई नेटवर्क नाम और पासवर्ड डालें
  while (WiFi.status() != WL_CONNECTED) {
    Serial.println("WiFi से स्थानांतरण कर रहा है ...");
    delay(500);
  }
  
  Serial.println("WiFi द्वारा संपर्क सफल।");
  }

void loop() {
  //यह हमारे HTML कोड को श्रेणियों में अलग करता है
  char *title_start = strstr(html, "<title>") + 7;
  char *title_end = strstr(html, "</title>");
  int title_length = title_end - title_start;
  char title[title_length + 1];
  strncpy(title, title_start, title_length);
  title[title_length] = '\0'; //शीर्षक में से अनुशंसा प्राप