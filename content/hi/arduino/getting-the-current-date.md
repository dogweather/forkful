---
title:    "Arduino: वर्तमान तारीख प्राप्त करना"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

आज की तारीख को प्राप्त करना आपके आर्डुइनो कोड में कई स्थानों पर उपयोगी हो सकता है, जैसे समय के साथ शुरुआत करने, लॉगिंग डेटा और समय पर आधारित निर्णय लेने में।

## कैसे करें

आप आर्डुइनो कोड में ```Arduino``` लाइब्रेरी का उपयोग करके वर्तमान तारीख और समय प्राप्त कर सकते हैं। नीचे दिए गए उदाहरण को फॉलो करें:

```arduino
#include <Arduino.h>
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  while(!Serial);
  setTime(12, 0, 0, 1, 1, 2021); // सेट करें(hour, minute, second, day, month, year)
  
  // तारीख और समय प्राप्त करें
  int currentHour = hour();
  int currentMinute = minute();
  int currentSecond = second();
  int currentDay = day();
  int currentMonth = month();
  int currentYear = year();
  
  // सीरियल मॉनिटर में दिखाएं
  Serial.print("वर्तमान समय: ");
  Serial.print(currentHour);
  Serial.print(":");
  Serial.print(currentMinute);
  Serial.print(":");
  Serial.print(currentSecond);
  Serial.println();
  Serial.print("वर्तमान तारीख: ");
  Serial.print(currentDay);
  Serial.print("/");
  Serial.print(currentMonth);
  Serial.print("/");
  Serial.print(currentYear);
}

void loop() {
}
```

नीचे का सैंपल आउटपुट आपको मॉनिटर पर दिखेगा:

```
वर्तमान समय: 12:0:0
वर्तमान तारीख: 1/1/2021
```

## गहराई में जाएं

```TimeLib.h``` लाइब्रेरी वर्तमान समय को माइक्रोसेकंड तक ही प्रकट कर सकती है, जो वास्तविक वर्तमान समय को सेकंड के हिसाब से प्रदर्शित करता है। इसके अतिरिक्त, यह आर्डुइनो कोड में सही समय को सेव करने और उसे बाद में उपयोग करने की भी सुविधा प्रदान करता है।

समय को सेट करने के लिए ```setTime()``` फंक्शन को उसी तरह उपयोग किया जा सकता है जैसे ऊपर दिखाया गया है। ह