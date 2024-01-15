---
title:                "मूल अनुमति के साथ एक http अनुरोध भेजना"
html_title:           "Arduino: मूल अनुमति के साथ एक http अनुरोध भेजना"
simple_title:         "मूल अनुमति के साथ एक http अनुरोध भेजना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

HTTP अनुरोध के साथ बेसिक प्रमाणीकरण भेजने में क्यों इंजीनियरिंग करने के लिए *क्यों* चुनें।

डेवलपर्स को कंप्यूटर नेटवर्क पर चलने का प्रकार और सुरक्षा के लिए अपनी वेब एप्लिकेशन्स को कॉन्त्रोल करने के लिए एचटीटीपी अनुरोध शामिल हो सकते हैं।

## कैसे करें

```arduino
#include <WiFi.h>
#include <HTTPClient.h>

void setup() {

  Serial.begin(115200);  //सीरियल बॉड रेट सेट करें
  WiFi.begin("SSID", "PASSWORD");  //अपने Wi-Fi के नाम और पासवर्ड डालें

  while (WiFi.status() != WL_CONNECTED) { //Wi-Fi कनेक्शन की प्रतीक्षा करें
    delay(500);  //लॉगिंग के लिए रुकें
    Serial.println("Connecting to WiFi..");
  }
  Serial.println("Connected to the WiFi network.");

  HTTPClient http;  //HTTPClient वर्तमान सत्र शुरू करें

  http.begin("http://example.com/api/");  //आपके यूआरएल से अपने एचटीटीपी सत्र शुरू करें
  http.setAuthorization("username", "password"); //यूजरनेम और पासवर्ड सेट करें
  int httpResponseCode = http.GET(); //HTTP अनुरोध भेजें
  String payload = http.getString();  //HTTP सत्र से डेटा प्राप्त करें

  Serial.println(httpResponseCode);  //ह्रष्ट पंक्ति दिखाएं
  Serial.println(payload);  //डेटा प्रिंट करें

  http.end();  //HTTP सत्र समाप्त करें
}

void loop() {
}
```

भागवाना, आसान! आप अब एक बेसिक प्रमाणीकृत एचटीटीपी अनुरोध भेज सकते हैं।

## गहराई-से-खोज

एचटीटीपी अनुरोध भेजने में बेसिक प्रमाणीकरण एक उत्तम सुरक्षा उपाय है। यदि आपके पास एक एपीआई उपलब्ध है, तो आप यूजरनेम और पासवर्ड सिर्फ उसके उपयोगकर्ता को दे सकते हैं जो इसका उपयोग करने के लिए अधिकारी हैं।

ड