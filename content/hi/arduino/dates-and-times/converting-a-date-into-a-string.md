---
date: 2024-01-20 17:36:24.087594-07:00
description: "\u0921\u0947\u091F \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\
  \u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u093E \u092E\
  \u0924\u0932\u092C \u0939\u0948 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u091C\
  \u093E\u0928\u0915\u093E\u0930\u0940 \u0915\u094B \u092A\u093E\u0920 \u0915\u0947\
  \ \u0930\u0942\u092A \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947\
  \ \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0924\
  \u093E\u0915\u093F \u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0906\u0938\u093E\
  \u0928\u0940 \u0938\u0947 \u092A\u0922\u093C\u093E \u091C\u093E \u0938\u0915\u0947\
  , \u0932\u0949\u0917\u2026"
lastmod: '2024-03-13T22:44:52.793599-06:00'
model: gpt-4-1106-preview
summary: "\u0921\u0947\u091F \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\
  \u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u093E \u092E\u0924\
  \u0932\u092C \u0939\u0948 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u091C\u093E\
  \u0928\u0915\u093E\u0930\u0940 \u0915\u094B \u092A\u093E\u0920 \u0915\u0947 \u0930\
  \u0942\u092A \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947\
  \ \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0924\
  \u093E\u0915\u093F \u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0906\u0938\u093E\
  \u0928\u0940 \u0938\u0947 \u092A\u0922\u093C\u093E \u091C\u093E \u0938\u0915\u0947\
  , \u0932\u0949\u0917\u2026"
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
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
