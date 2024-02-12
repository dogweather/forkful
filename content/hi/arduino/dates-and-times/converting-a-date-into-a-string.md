---
title:                "तारीख को स्ट्रिंग में बदलना"
aliases:
- /hi/arduino/converting-a-date-into-a-string/
date:                  2024-01-20T17:36:24.087594-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डेट को स्ट्रिंग में बदलने का मतलब है तारीख की जानकारी को पाठ के रूप में बदलना। प्रोग्रामर्स इसे इसलिए करते हैं ताकि तारीख को आसानी से पढ़ा जा सके, लॉग किया जा सके या यूजर इंटरफेस में दिखाया जा सके।

## How to: (कैसे करें:)
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("RTC is NOT running!");
    while (1);
  }

  if (rtc.lostPower()) {
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  char dateStr[20];
  snprintf(dateStr, sizeof(dateStr), "%02d/%02d/%04d %02d:%02d:%02d", now.day(), now.month(), now.year(), now.hour(), now.minute(), now.second());
  
  Serial.println(dateStr);
  
  delay(1000);
}
```
साउटपुट: `05/03/2023 15:20:56`

## Deep Dive (गहराई से जानकारी):
डेट को स्ट्रिंग में बदलना बहुत पुराना और आम काम है, जो प्रोग्रामिंग की शुरुआत से होता आ रहा है। Arduino में RTC (Real Time Clock) मॉड्यूल का उपयोग करते हुए वास्तविक समय की जानकारी प्राप्त की जाती है, जैसे DS3231। `snprintf()` फ़ंक्शन इस्तेमाल करके हम तारीख और समय को एक ख़ास फ़ॉर्मेट में स्ट्रिंग द्वारा प्रिंट कर सकते हैं। विकल्प के रूप में, अन्य libraries भी मौजूद हैं जैसे `TimeLib.h` जो यह काम कर सकती हैं। अतिरिक्त implementation details में, हमें `DateTime` ऑब्जेक्ट का इस्तेमाल करने का ध्यान रखना होता है, जो कि RTC लाइब्रेरी से आता है और वास्तविक समय की जानकारी रखता है।

## See Also (और देखें):
- Arduino `RTClib` Library: https://github.com/adafruit/RTClib
- Arduino Reference for `snprintf()`: https://www.arduino.cc/reference/en/language/functions/characters/snprintf/
- `TimeLib.h` Library: https://www.pjrc.com/teensy/td_libs_Time.html
