---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:01.435760-07:00
description: "\u0915\u0948\u0938\u0947: \u0924\u0943\u0924\u0940\u092F-\u092A\u0915\
  \u094D\u0937 \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F \u0915\u0947\
  \ \u092C\u093F\u0928\u093E \u092A\u094D\u0930\u0924\u094D\u092F\u0915\u094D\u0937\
  \ \u0926\u0943\u0937\u094D\u091F\u093F\u0915\u094B\u0923."
lastmod: '2024-03-13T22:44:52.789948-06:00'
model: gpt-4-0125-preview
summary: "\u0924\u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092A\u0941\u0938\
  \u094D\u0924\u0915\u093E\u0932\u092F \u0915\u0947 \u092C\u093F\u0928\u093E \u092A\
  \u094D\u0930\u0924\u094D\u092F\u0915\u094D\u0937 \u0926\u0943\u0937\u094D\u091F\u093F\
  \u0915\u094B\u0923."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0924\u093E\
  \u0930\u0940\u0916 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
weight: 30
---

## कैसे:
तृतीय-पक्ष पुस्तकालय के बिना प्रत्यक्ष दृष्टिकोण:

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // उदाहरण तारीख स्ट्रिंग YYYY-MM-DD प्रारूप में
  String dateString = "2023-04-01"; 

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // पार्स किए गए घटकों के साथ एक DateTime ऑब्जेक्ट को प्रारम्भ करना
  DateTime parsedDate(year, month, day);
  
  Serial.print("पार्स की गई तारीख: ");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

नमूना आउटपुट:
```
पार्स की गई तारीख: 2023/4/1
```

अधिक जटिल पार्सिंग परिदृश्यों के लिए तृतीय-पक्ष पुस्तकालय का उपयोग (*ArduinoJson* जैसे एक JSON प्रतिक्रिया से तारीख प्राप्त करने के लिए):

सबसे पहले, Arduino लाइब्रेरी मैनेजर के माध्यम से ArduinoJson लाइब्रेरी इंस्टॉल करें।

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // JSON प्रतिक्रिया का अनुकरण
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // तारीख स्ट्रिंग निकालना
  const char* date = doc["date"];

  // पहले की तरह स्ट्रिंग से तारीख पार्स करें
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("JSON से पार्स की गई तारीख: ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

नमूना आउटपुट:
```
JSON से पार्स की गई तारीख: 2023/7/19
```
