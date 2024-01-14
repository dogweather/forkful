---
title:                "Arduino: भविष्य या अतीत में दिनांक की गणना"
simple_title:         "भविष्य या अतीत में दिनांक की गणना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

अर्डुइनो प्रोग्रामिंग क्यों की जाती है? आप अपने प्रोजेक्ट में एक तारीख को भविष्य में या भूतकाल में गणना करना चाहते होंगे। इससे आप अपने प्रोजेक्ट का समयांतर बना सकते हैं या अपने स्केच में मध्यावधि समय को गणना कर सकते हैं। 

## कैसे करें

आपको अपने अर्डुइनो बोर्ड में Real Time Clock (RTC) मॉड्यूल का उपयोग करना होगा। यह आपको वर्तमान दिनांक और समय को पढ़ने और आगामी या अतीत में समय गणना करने की अनुमति देता है। यहां हम आपको भविष्य की तारीख के लिए कोड दर्शाएंगे: 

```Arduino
#include <Wire.h>
#include "RTClib.h"

RTC_DS1307 rtc;

void setup () {
  Serial.begin(9600);
  if (! rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  if (! rtc.isrunning()) {
    Serial.println("RTC is NOT running!");
    // following line sets the RTC to the date & time this sketch was compiled
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
    // This line sets the RTC with an explicit date & time, for example to set
    // January 21, 2014 at 3am you would call:
    // rtc.adjust(DateTime(2014, 1, 21, 3, 0, 0));
  }
  DateTime now = rtc.now();

  // replace the following line with whatever date you'd like
  DateTime futureDate (now.year() + 1, 1, 1, 0, 0, 0);

  Serial.print("The current date is: ");
  Serial.print(now.year());
  Serial.print("-");
  Serial.print(now.month());
  Serial.print("-");
  Serial.println(now.day());
  
  Serial.print("The future date is: ");
  Serial.print(futureDate.year());
  Serial.print("-");
  Serial.print(futureDate.month());
  Serial.print("-");
  Serial.println(futureDate.day());
}

void loop () {
  // do nothing
}
```

जब आप इन कोड्स को अपने अर्डुइनो बोर्ड पर अपलोड करेंगे, सीरियल मॉनिटर पर आप वर्तमान और भविष्य की तिथियों को देख सकते हैं। आप चाहें तो इन तिथियों को हैप्टोनिक फार्मेट में भी लेख सकते हैं। 

## गहराई में जाएं

जब हम समय गणना करते हैं, हमें समय के साथ गहराई में जाना होत