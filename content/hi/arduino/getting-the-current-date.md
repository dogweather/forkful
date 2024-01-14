---
title:                "Arduino: तारीख को प्राप्त करना"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# क्यों

क्या आपको अपने आर्डूइनो प्रोजेक्ट में वर्तमान तारीख की आवश्यकता है? शायद आप अपने प्रोजेक्ट में कोई टाइमस्टैम्प रिकॉर्ड करना चाहते हो या शायद आप अपने प्रोजेक्ट में एक टाइमर या कैलेंडर शामिल करना चाहते हो। यहां हम आपको बताएंगे कि आर्डूइनो प्रोग्रामिंग में वर्तमान तारीख को कैसे प्राप्त कर सकते हैं।

# कैसे करें

हमारे पास विभिन्न तरीके हैं जिनका उपयोग करके आप आर्डूइनो में वर्तमान तारीख को प्राप्त कर सकते हैं। यहां हम आपको दो उदाहरण दिखाएंगे। एक में हम आर्डूइनो बोर्ड के साथ एक डीएस3231 रीयल टाइम क्लॉक मॉड्यूल का उपयोग करेंगे और दूसरे में हम विशेष अंतराल गुणक RTClib लाइब्रेरी का उपयोग करेंगे। कोड ब्लॉक में सरल उदाहरण दिखाए गए हैं, ताकि आप आसानी से इन तरीकों का उपयोग कर सकें।

```Arduino
// डीएस3231 मॉड्यूल का उपयोग करके वर्तमान तारीख को लेना
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup () {
  Serial.begin(9600);
  Wire.begin();
  rtc.begin();
}

void loop () {
  DateTime now = rtc.now();
  Serial.print(now.day(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.year(), DEC);
  Serial.print(' ');
  Serial.print(now.hour(), DEC);
  Serial.print(':');
  Serial.print(now.minute(), DEC);
  Serial.print(':');
  Serial.print(now.second(), DEC);
  Serial.println();
  delay(1000);
}
```

```Arduino
// RTClib लाइब्रेरी का उपयोग करके वर्तमान तारीख जानना
#include <Wire.h>
#include "RTClib.h"

RTC_DS1307 rtc;

void setup () {
  Serial.begin(9600);
  Wire.begin();
  rtc.begin();
}

void loop () {
  DateTime now = rtc.now();
  uint8_t second = now.second();
  uint8_t minute = now.minute();
  uint8_t hour = now.hour();
  uint