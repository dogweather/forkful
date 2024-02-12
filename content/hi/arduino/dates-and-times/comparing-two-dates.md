---
title:                "दो तारीखों की तुलना"
aliases:
- /hi/arduino/comparing-two-dates.md
date:                  2024-01-20T17:33:09.133204-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

दो तारीखों की तुलना का मतलब है उन्हें आपस में परखना, जानना कि कौनसी पहले है और कौनसी बाद में। प्रोग्रामर्स ऐसा ईवेंट्स, डेडलाइन्स और टाइमस्टैम्प्स को मैनेज करने, और समयांतराल की गणना करने के लिए करते हैं।

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
