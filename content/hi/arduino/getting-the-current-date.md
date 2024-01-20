---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
Arduino में वर्तमान तारीख जानने का कार्य एक समय संबंधी कुछ डेटा को प्राप्त करना होता है। इसे प्रोग्रामर्स उन सभी उद्देश्यों के लिए करते हैं जिन्हें वे समय के साथ संबद्ध करने के लिए आवश्यक मानते हैं, जैसे कि डेटा लॉगिंग और गतिविधियों की टाइमिंग।

## कैसे करें:
Arduino में, हम इसे `RTC` (Real Time Clock) मॉड्यूल के साथ कर सकते हैं। यहाँ `DS1307 RTC` का उदाहरण है:

```Arduino
#include <Wire.h>
#include "RTClib.h"

RTC_DS1307 rtc;

void setup () {
  while (!Serial);
  if (! rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
  if (! rtc.isrunning()) {
    Serial.println("RTC is NOT running!");
    // following line sets the RTC to the date & time this sketch was compiled
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop () {
  DateTime now = rtc.now();
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);
  delay(1000);
}
```
चलायें और देखें की आपको सही तारीख मिलती है।

## गहरी जानकारी
वर्तमान तारीख के आविष्कार के प्रशासनिक और वैज्ञानिक उपयोगों के लिए एक बहुत महत्वपूर्ण उपकरण है। जैसा आपने ऊपर देखा, हमने `DS1307 RTC` मॉड्यूल इस्तेमाल किया। विकल्प के रूप में, Arduino के लिए `DS3231` और `PCF8563` जैसे अन्य RTCs भी हैं जिन्हें आप इस्तेमाल कर सकते हैं।

## अधिक जानें
यदि आप अधिक जानना चाहते हैं तो निम्नलिखित लिंक पर जाएँ:
- [Arduino Website](https://www.arduino.cc/)
- [Arduino Time Library](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Arduino Playground](https://playground.arduino.cc/Main/GeneralCodeLibrary#TOC-Real-Time-Clock-RTC-)