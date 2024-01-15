---
title:                "वर्तमान तिथि प्राप्त करना"
html_title:           "Arduino: वर्तमान तिथि प्राप्त करना"
simple_title:         "वर्तमान तिथि प्राप्त करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## इसलिए 
क्या आप यह जानने के लिए उत्सुक हैं कि हम प्रोग्रामिंग में तारीख को कैसे प्राप्त कर सकते हैं? अर्डुइनो कुशल प्रोग्रामर ही नहीं, लेकिन क्या आप इस अंतरिक्ष कार्यक्रमी कंपनी के शानदार उपकरण के साथ हुए बैतैं हैं? यदि ऐसा है. आपको सबसे बढ़िया प्रथम कदम समझ सकते हैं सुरुआत करने के लिए इस "विस्तार में" से पढ़ सकते हैं कि आप कैसे अपने अर्डुइनो के साथ करेंगे, तारीख को प्राप्त करने का प्रयास "रीले हो रहा हे, आपको प्राप्त होता है ना और समय और तारीख पर जानना है कि आप अपने प्रोजेक्ट में संचालकता कितना डता है

## कैसे करें
```Arduino
#include <RTClib.h>
RTC_DS1307 rtc;
DateTime now;

void setup() {
  rtc.begin();
}

//getting current date and time
void loop() {
  now = rtc.now();
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

**आउटपुट:**

2021/5/18 13:30:20

## दीप डाइव
अर्डुइनो में, मूल्य के लिए आप प्रोग्रामिंग किए गए मूल्य कब त्रुति वक्त होगा, हो सकता है कि आपको अपने सर्किट में भी तारीख और समय को संचालित करने के लिए तारीख समय कार्यक्रमी की आवश्यकता होती है. साध्य हो में भव्य अनुभव है कि आप अपने राजमार्ग का हि योजना करने में मदद करता है, तारीख प्राप्त करने के लिए इस आर्टिकल के साथ आपको प्रैक्टिक्य हैंड - सो से सीख सकते हैं. यदि आप