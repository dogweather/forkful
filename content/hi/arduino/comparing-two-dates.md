---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

"दो तारिकों की तुलना" का मतलब होता है कि ऐर्डुइनो समझे कि कौनसी तारिख पहले आती है और कौनसी बाद में। प्रोग्रामर्स इसे करते हैं क्योंकि समय और तारिखों का अनुसरण करना, स्केड्यूलिंग, लॉगिंग और डेटा संग्रहण के लिए महत्वपूर्ण है।

## कैसे करें:

डेट के समय के दो भागों की तुलना करने के लिए **getTime()** मैथॉड का प्रयोग करें। 

```Arduino
#include <TimeLib.h>

time_t t1 = now(); //एक तारिख और समय
delay(1000); //दूसरी तारिख और समय तक का विलंब
time_t t2 = now();

if(t1 < t2){
  Serial.println("t1 < t2");
} else {
  Serial.println("t1 >= t2");
}
```

"t1 < t2" का आउटपुट देखें।

## गहराई में:

तारीखों की तुलना का इतिहास कंप्यूटर विज्ञान की शुरुआत से ही जुड़ा है। यह कनसेप्ट क्लॉड डेटा संग्रहण, IoT, और कई अन्य उपयोगों के लिए महत्वपूर्ण है।

वैकल्पिक तरीके में, आप "==" ऑपरेटर का भी उपयोग कर सकते हैं दो तारीखों के बीते हुए मिलिसेकंड की तुलना करने के लिए। 

Arduino बोर्ड में RTC (Real-Time Clock) module का उपयोग दिनांक और समय की जांच करने के लिए किया जाता है, जिसे आप अद्यतित कर सकते हैं या अपने कोड में शामिल कर सकते हैं। 

## देखें भी:

1. [Arduino Official Time Library Doc](https://www.arduino.cc/en/reference/time)
2. [Arduino Libraries](https://www.arduino.cc/en/guide/libraries)
3. [Arduino and Real Time Clock](https://randomnerdtutorials.com/guide-for-real-time-clock-rtc-module-with-arduino-ds1307-and-ds3231/)