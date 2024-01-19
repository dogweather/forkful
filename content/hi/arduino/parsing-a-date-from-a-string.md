---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
तारिख को स्ट्रिंग से पार्स करना मतलब होता है एक वाक्यांश या शब्द से तारिख को प्राप्त करना। यह इसलिए जरूरी होता है क्यूंकि प्रोग्रामर्स को कभी-कभी डेटा को इस तरह से संग्रहित और प्रबंधित करने की आवश्यकता होती है ताकि वे उन्हें आसानी से पार्स कर सकें। 

## कैसे:
यहां एक साधारण प्रोग्राम है जो स्ट्रिंग से तारिख पार्स करता है:

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) ; // wait until Arduino Serial Monitor opens
  setSyncProvider(getTeensy3Time);
  if(timeStatus()!= timeSet) 
     Serial.println("Unable to sync with RTC");
  else
     Serial.println("RTC has set the system time");     
}

time_t getTeensy3Time()
{
  return Teensy3Clock.get();
}

void loop() {
  digitalClockDisplay();
  delay(1000);
}

void digitalClockDisplay() {
  char dateString[12];  //HH:MM:SS DD-MM-YYYY
  sprintf(dateString, "%02d:%02d:%02d %02d-%02d-%04d", hour(), minute(), second(), day(), month(),year());
  Serial.println(dateString);
}
```
उपरोक्त कोड स्ट्रिंग से तारीख को पार्स करता है और इसे फॉर्मेट करता है ताकि इसे छापा जा सके।

## गहराई में:
तारीख को स्ट्रिंग से पार्स करना कोई नई तकनीक नहीं है। इसका उपयोग डेटाबेस, वेब अनुप्रयोगों और ऐप्स में बहुत सालों से हो रहा है संगठनात्मक और सुविधाजनक संग्रहण के लिए। इसके विकल्प आधारित रूप से उस प्रोग्रामिंग भाषा पर निर्भर करते हैं जिसका आप उपयोग कर रहे हैं। 

Arduino में, `TimeLib` लाइब्रेरी का उपयोग करके हम आसानी से स्ट्रिंग से तारीख पार्स कर सकते हैं। इसमें `hour()`, `minute()`, `second()`, `day()`, `month()`, `year()` जैसे कई उपयोगी फ़ंक्शन्स शामिल हैं जो हमें विशेष तारीख और समय घटकों को निकालने में मदद करते हैं।

## और जानें:
अगर आप इस विषय पर और अधिक जानना चाहते हैं, तो आप निम्नलिखित संसाधनों का पता लगा सकते हैं:

1. Arduino Time Library Documentation: https://www.arduino.cc/reference/en/libraries/time/
2. More on Parsing Dates: https://www.arduino.cc/reference/en/libraries/rtc-ds1307/
3. Real Life Applications and Tutorials: https://learn.adafruit.com/adafruit-ds1307-real-time-clock-module-arduino/