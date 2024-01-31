---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:13:37.472139-07:00
html_title:           "C: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"

category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
मौजूदा तारीख पता करना यानी आपका Arduino सिस्टम वर्तमान दिन, माह, और वर्ष जान सके। प्रोग्रामर्स इसे लॉगिंग, समय-निर्भर इवेंट्स और टाइम-स्टैम्प्स के लिए उपयोग करते हैं।

## How to: (कैसे करें:)
Arduino में वर्तमान तारीख प्राप्त करने के लिए, आपको RTC (Real Time Clock) मॉड्यूल की आवश्यकता होगी। नीचे DS3231 RTC मॉड्यूल का उपयोग करते हुए एक साधारण कोड उदाहरण दिया गया है:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("RTC शुरू नहीं हो पाया!");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC ने शक्ति खो दी है, समय को रीसेट करें!");
    // इस लाइन को अपने समय के हिसाब से बदलें:
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  Serial.print(" ");
  Serial.print(now.hour(), DEC);
  Serial.print(':');
  Serial.print(now.minute(), DEC);
  Serial.print(':');
  Serial.print(now.second(), DEC);
  Serial.println();

  delay(1000);
}
```

## Deep Dive (गहराई से जानकारी)
RTC मॉड्यूल Arduino को समय और तारीख बताता है और यह बिजली जाने के बाद भी काम करता रहता है। DS3231 एक पॉपुलर RTC चिप है क्योंकि यह काफी सटीक है और तापमान परिवर्तनों से प्रभावित नहीं होता। विकल्प के रूप में, DS1307 और DS3232 भी उपलब्ध हैं। 

यह ध्यान रखना महत्वपूर्ण है कि RTC मॉड्यूल को पहली बार शुरू करते समय समय और तारीख सेट करनी पड़ती है। `rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));` लाइन इस उद्देश्य के लिए है।

## See Also (देखें भी)
- RTClib library का documentation: https://github.com/adafruit/RTClib
- Arduino Time Library: https://www.pjrc.com/teensy/td_libs_Time.html
- Arduino के साथ NTP सर्वर से समय सिंक्रोनाइज़ेशन: https://lastminuteengineers.com/esp8266-ntp-server-date-time-tutorial/
