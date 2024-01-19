---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

माइक्रोकंप्यूटर प्रोग्रामिंग में 'स्ट्रिंग की लंबाई निकालना' से उद्देश्य होता है एक विशेष स्ट्रिंग में कितने अक्षर होते हैं, उनकी गणना करना। यह पहचानने के लिए महत्वपूर्ण होता है कि कोई स्ट्रिंग सही आकार में है या नहीं, क्या इसमें सही डाटा है, और यह कहां समाप्त होता है।

## कैसे:

```C
#include <stdio.h>
#include <string.h>

void main() {
    char str[] = "Hello, World!";
    int length = strlen(str);

    printf("The length of the string is %d\n", length);
}
```

आउटपुट: 
```
The length of the string is 13
```

## गहरी डाइव

जब सी प्रोग्रामिंग लैंग्वेज की खोज हुई, तब 'स्ट्रिंग' में किसी विशेष अक्षर की तलाश करने या उसकी लंबाई जानने के लिए 'strlen' फ़ंक्शन का विकास हो गया। यद्यपि इसके विकल्प जैसे `strnlen()` भी हैं, जिसे बाद में जोड़ा गया। फंक्शन strlen() हर character को गिनता है जब तक कि यह null character ('\0') नहीं मिलता। यही कारण है कि ध्यान देना महत्वपूर्ण होता है, स्ट्रिंग को सही ढंग से निर्धारित करने को, सही null character के साथ।

## देखिए भी

1. अधिक विवरण के लिए, चेक करें [सी लैंग्वेज स्ट्रिंग फंक्शन्स](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
2. [सी प्रोग्रामिंग स्ट्रिंग्स ट्यूटोरियल](https://www.learn-c.org/en/Strings)
3. [स्ट्रिंग लेंग्थ फंक्शन जानकारी - Cplusplus.com](https://www.cplusplus.com/reference/cstring/strlen/)