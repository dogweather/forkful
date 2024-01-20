---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग इंटरपोलेशन एक प्रक्रिया है जिसमें आप एक स्ट्रिंग में किसी अन्य मान को जोड़ सकते हैं। प्रोग्रामर इसे कोड को पढ़ने और लिखने को आसान बनाने के लिए करते हैं।

## कैसे करें:

```Arduino
char name[] = "Rajan";
char greeting[50];   
sprintf(greeting, "नमस्ते, %s", name);
Serial.println(greeting);
```

यह कोड "नमस्ते, Rajan" छापेगा।

## गहराई में:

1. ऐतिहासिक संदर्भ: स्ट्रिंग इंटरपोलेशन का प्रयोग अधिक स्पष्ट और पठनीय कोड लिखने के लिए किया जाता है। यह 1960 के दशक से इस्तेमाल हो रहा है।

2. विकल्प: आप इंटरपोलेशन के बिना भी स्ट्रिंग संयोजन कर सकते हैं, लेकिन यह अधिक जटिल हो सकता है।

3. कार्यान्वयन विवरण: आर्डुइनो में, `sprintf()` फ़ंक्शन का उपयोग स्ट्रिंग इंटरपोलेशन के लिए किया जाता है। यह एक विशेष प्रारूप स्ट्रिंग को साधारण स्ट्रिंग में परिवर्तित करता है। 

## यह भी देखें:

1. [Arduino String Interpolation](https://www.arduino.cc/en/Tutorial/StringAdditionOperator)

2. [Arduino sprintf()](https://www.arduino.cc/reference/tr/language/functions/characters/printf/)