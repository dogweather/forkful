---
date: 2024-01-20 17:33:09.133204-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: ."
lastmod: '2024-03-13T22:44:52.795238-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
weight: 27
---

## कैसे करें:
```arduino
#include <RTClib.h>  // RTC library
RTC_DS3231 rtc;     // RTC object

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("RTC not found!");
    while (1);
  }

  DateTime now = rtc.now();  // वर्तमान तारीख और समय
  DateTime then(2023, 4, 1, 0, 0, 0);  // पिछली निर्धारित तारीख और समय

  if (now > then) {
    Serial.println("वर्तमान समय पिछले समय से आगे है।");
  } else if (now == then) {
    Serial.println("दोनों समय समान हैं।");
  } else {
    Serial.println("वर्तमान समय पिछले समय से पीछे है।");
  }
}

void loop() {
  // यहां लूप खाली है क्योंकि हम एक बार ही तारीखों की तुलना कर रहे हैं।
}
```

## गहराई में:
दो तारीखों की तुलना हमेशा से एक महत्वपूर्ण कार्य रहा है, कैलेंडर और घड़ियों के आविष्कार से ही। ऐतिहासिक तौर पर, लोग सूर्य और तारों की गति को देखकर समय का पता लगाते थे। आज के डिजिटल युग में, रियल-टाइम क्लॉक (RTC) मॉड्यूल्स जैसे DS3231 का इस्तेमाल करके माइक्रोकंट्रोलर्स में समय और तारीख की सटीकता में सुधार हुआ है। दो तारीखें तुलना करने के लिए और भी तरीके हैं, जैसे कि मिलीसेकंड्स में तारीखों को बदलना और फिर तुलना करना। हालांकि, यह जानकारी मौजूदा उदाहरण से ज्यादा जटिल हो सकती है।

## और भी देखें:
- Arduino's official RTC library: https://www.arduino.cc/en/Reference/RTC
- DS3231 datasheet for specification details: https://datasheets.maximintegrated.com/en/ds/DS3231.pdf
- Time library for Arduino: https://www.arduino.cc/en/Reference/Time
