---
title:                "Bhavishya ya Bhoot ki Tarikh ka Ganana"
html_title:           "Arduino: Bhavishya ya Bhoot ki Tarikh ka Ganana"
simple_title:         "Bhavishya ya Bhoot ki Tarikh ka Ganana"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# क्या और क्यों?
पीछे या आगामी में एक तारीख को गणना करना एक आम कार्य है जो कि प्रोग्रामर इस्तेमाल करते हैं। इसे एक संज्ञात तारीख तक पहुंचने के लिए या एक निश्चित तारीख से पीछे जाने के लिए किया जाता है।

# कैसे करें?
Arduino के साथ एक तारीख को आगे या पीछे गणना करने के लिए निम्नलिखित कोड का उपयोग कर सकते हैं।

```
Arduino या संबंधित कोड

#include <RTClib.h>

DateTime currentDate = DateTime(year, month, day, hours, minutes, seconds);
DateTime futureDate = currentDate + TimeSpan(days, hours, minutes, seconds);
DateTime pastDate = currentDate - TimeSpan(days, hours, minutes, seconds);

Serial.print(futureDate.unixtime());
Serial.print(pastDate.unixtime());
```

यह आउटपुट निम्नवत होगा:

```
XXXXXXX
XXXXXXX
```

यहां, XXXXXXX तारीख और समय को Unix फॉर्मेट में लिखते हैं।

# गहराई गवाह
इतिहास की संदर्भ में, प्रोग्रामर्स एक तारीख को गणना करने के लिए अन्य तरीकों पर भी काम कर सकते हैं। यह साधारणतः इस्तेमाल किया जाता है जब आपको प्रोग्राम को एक तारीख या दो तारीखों के बीच अंतर की आवश्यकता हो।

इस काम को करने के लिए, आपको सबसे पहले Arduino को एक RTC (Real Time Clock) शॉल्डर के साथ कनेक्ट करना होगा। RTC शॉल्डर में एक टाइमर होता है जो आपको सही समय और दिनांक की जानकारी देता है। उपयोगकर्ता का स्थानांतरण प्रणाली या समय क्षेत्र के आधार पर, RTC शॉल्डर को डेटा होल्ड संचालित करता है और सक्रिय रखता है।

# अधिक जानकारी के लिए
आप अधिक जानकारी के लिए निम्न लिंकों पर जा सकते हैं:

- Arduino की आधिकारिक वेबसाइट: https://www.arduino.cc/
- RTClib लाइब्रेरी: https://github.com/adafruit/RTClib
- समय को Unix फॉर्मेट में मापना: https://en.wikipedia.org/wiki/Unix_time