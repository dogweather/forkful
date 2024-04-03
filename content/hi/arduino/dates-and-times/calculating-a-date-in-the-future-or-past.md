---
date: 2024-01-20 17:31:28.509004-07:00
description: "\u0915\u0948\u0938\u0947: (How to:) Arduino \u092C\u094B\u0930\u094D\
  \u0921 \u092A\u0930 \u0906\u092A \"TimeLib.h\" \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932\
  \ \u0915\u0930\u0915\u0947 \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0924\u093E\
  \u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\u093E \u0915\u0930 \u0938\u0915\
  \u0924\u0947 \u0939\u0948\u0902."
lastmod: '2024-03-13T22:44:52.796920-06:00'
model: gpt-4-1106-preview
summary: "Arduino \u092C\u094B\u0930\u094D\u0921 \u092A\u0930 \u0906\u092A \"TimeLib.h\"\
  \ \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u093E \u0907\u0938\
  \u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947 \u0906\u0938\u093E\
  \u0928\u0940 \u0938\u0947 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\
  \u0928\u093E \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902."
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
weight: 26
---

## कैसे: (How to:)
Arduino बोर्ड पर आप "TimeLib.h" लाइब्रेरी का इस्तेमाल करके आसानी से तारीख की गणना कर सकते हैं:

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(23, 59, 50, 4, 1, 2021); // 23:59:50, 4 January 2021
}

void loop() {
  time_t now = now();
  
  // भविष्य में 10 दिन जोड़ना
  time_t futureDate = now + 10 * SECS_PER_DAY;
  Serial.print("Future Date: ");
  Serial.println(day(futureDate));
  Serial.print("/");
  Serial.println(month(futureDate));
  Serial.print("/");
  Serial.println(year(futureDate));
  Serial.println();

  // अतीत में 10 दिन घटाना
  time_t pastDate = now - 10 * SECS_PER_DAY;
  Serial.print("Past Date: ");
  Serial.println(day(pastDate));
  Serial.print("/");
  Serial.println(month(pastDate));
  Serial.print("/");
  Serial.println(year(pastDate));
  Serial.println();

  delay(10000); // Update every 10 seconds
}
```

## गहराई में: (Deep Dive)
तारीखों को गणना करना कंप्यूटर साइंस और प्रोग्रामिंग के पुराने समस्याओं में से एक है। यह ज्यादातर प्रोग्रामर लाइब्रेरी या टाइम सर्विस API के जरिए करते हैं जो कि टाइम जोन्स, लीप ईयर और दूसरी जटिलताओं को संभालते हैं। Arduino में `TimeLib.h` एक ऐसी ही शक्तिशाली लाइब्रेरी है जो समय और तारीख की गणना में मदद करती है।

'Unix Time' प्रणाली भी लोकप्रिय है, जहां समय को 1 जनवरी 1970 से सेकंड्स में मापा जाता है। लेकिन Arduino पर डेट और टाइम के साथ काम करते वक्त, मेमोरी और प्रोसेसिंग पावर कम होने की वजह से 'TimeLib.h' जैसी लाइब्रेरीज को पसंद किया जाता है।

## संबंधित स्रोत: (See Also)
- Arduino Time Library: https://www.arduino.cc/en/Reference/Time
- Unix Time and Date Command: https://www.unixtimestamp.com/
- NTP Client for Arduino: https://github.com/arduino-libraries/NTPClient

अधिक जानकारी और कोड उदाहरण के लिए इन स्रोतों पर जाएं। यह आपकी समझ को और भी बेहतर बनाने में मदद करेंगे।
