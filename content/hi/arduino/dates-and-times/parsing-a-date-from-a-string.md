---
title:                "स्ट्रिंग से तारीख पार्स करना"
aliases:
- /hi/arduino/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:01.435760-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से तारीख पार्स करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Arduino में एक स्ट्रिंग से तारीख पार्स करना समयसीमा के घटकों (वर्ष, महीना, दिन) को एक पाठ रूपांतरण से निकालकर उन्हें ऐसे प्रारूप में परिवर्तित करना शामिल है जिसका उपयोग स्केचेस में समय रखने, तुलना करने या हेरफेर के लिए किया जा सके। प्रोग्रामर्स अक्सर इस कार्य को वास्तविक समय घड़ियों, लॉगर्स के साथ इंटरफ़ेस करने या वेब एपीआई और यूजर इंटरफेसेज से इनपुट प्रोसेस करने के लिए करते हैं जहाँ तारीखें पठनीय प्रारूप में प्रस्तुत की जा सकती हैं।

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
