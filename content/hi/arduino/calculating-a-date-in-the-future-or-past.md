---
title:                "भविष्य या अतीत में एक तारीख की गणना"
html_title:           "Arduino: भविष्य या अतीत में एक तारीख की गणना"
simple_title:         "भविष्य या अतीत में एक तारीख की गणना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Future और Past Date की Calculation कैसे करे Arduino से?

## What & Why? (यह क्या है और क्यों?)

Future या Past Date की calculation से हम मतलब रखते हैं एक तारीख की गणना करना, जो अब से कुछ समय बाद या पहले हो। प्रोग्रामर्स इसको इसलिए करते हैं क्योंकि इससे उन्हें time-sensitive  tasks और event tracking में मदद मिलती हैं। 

## How to: (कैसे करें)

चलिए अब हम देखते हैं कि कैसे Arduino काम करता है इसे समझने के लिए। 

```Arduino 
#include <TimeLib.h>
void setup() {
  setTime(12, 0, 0, 1, 1, 2021); //HH:MM:SS, DD:MM:YYYY
  Serial.begin(9600);
}
void loop() {
  time_t futureTime = now() + (1 * DAYS); 
  Serial.print("Current Date: ");
  Serial.print(day());
  Serial.print("/");
  Serial.print(month());
  Serial.print("/");
  Serial.println(year());
  Serial.print("Future Date: ");
  Serial.print(day(futureTime));
  Serial.print("/");
  Serial.print(month(futureTime));
  Serial.print("/");
  Serial.println(year(futureTime));
  delay(10000);
}
```
Output :
```Arduino
Current Date: 1/1/2021
Future Date: 2/1/2021
```
## Deep Dive (गहरी जानकारी)

इस concept का उपयोग Arduino Programming में historical context तैयार करने, date-based operations को execute करने और temporal logic पर base किए गए systems को manage करने में किया जाता है। 

विकल्पों के हिसाब से, आप और भी libraries जैसे कि <DateTime.h> और <RTClib.h> का उपयोग कर सकते हैं। ये Arduino के लिए अधिक विस्तृत functionality provide करते हैं। 

Date calculation की implementation details के लिए TimeLib library 'now()' function का उपयोग करती है जो current time को UNIX timestamp के रूप में return करती है। इसे एक Day (expressed as seconds) से add किया जाता है future date जनरेट करने के लिए।

## See Also (संबंधित जानकारी )

1. [Arduino Time Library](https://www.arduinolibraries.info/libraries/time)
2. [Arduino Date Calculation Example](http://playground.arduino.cc/Code/time)
3. [Arduino DateTime Library](https://github.com/ropg/ezTime) 
4. [Arduino RTC library](https://www.arduino.cc/reference/en/libraries/rtclib/)