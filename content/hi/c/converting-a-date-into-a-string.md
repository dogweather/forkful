---
title:                "C: एक तारीख को स्ट्रिंग में रूपांतरण करना"
simple_title:         "एक तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों
कई बार, हमें तारीख को स्ट्रिंग में रूपांतरित करने की जरूरत होती है, जैसे कि किसी स्प्रेडशीट में रजिस्टर्ड तारीखों को स्ट्रिंग के रूप में प्रदर्शित करने के लिए। इसके अलावा, कुछ प्रोग्राम में भी तारीख को स्ट्रिंग में रूपांतरित करने की जरूरत पड़ सकती है। इस ब्लॉग पोस्ट में, हम दिखाएंगे कि एक तारीख को स्ट्रिंग में कैसे रूपांतरित किया जा सकता है।

## कैसे करें
```C
#include <stdio.h>
#include <string.h>
#include <time.h>

int main() {
    // तारीख और समय को लेना
    time_t now = time(NULL);
    struct tm *current_time = localtime(&now);
    // तारीख को स्ट्रिंग में रूपांतरित करना
    char date_string[50];
    strftime(date_string, sizeof(date_string), "%B %d, %Y", current_time);
    // स्ट्रिंग को प्रिंट करना
    printf("वर्तमान तारीख: %s\n", date_string);
    return 0;
}
```

कोड को कॉंपाइल और एक्जीक्यूट करने के बाद, आपको इस तरह का आउटपुट मिलना चाहिए:
```
वर्तमान तारीख: मई 24, 2021
```

यहां हम `time()` और `localtime()` का उपयोग करके वर्तमान तारीख और समय को लेते हैं। इसके बाद, `strftime()` फ़ंक्शन का उपयोग करके हम तारीख को हमारे चयनित फॉर्मेट में स्ट्रिंग में रूपांतरित करते हैं। आप भी अपने चयन के अनुसार अन्य फ़ॉर्मेट का उपयोग कर सकते हैं।

## गहराई से जाएं
यदि आप अधिक जानना चाहते हैं, तो `strftime()` फ़ंक्शन के [डॉक्यूमेंटेशन](https://www.cplusplus.com/reference/ctime/strftime/) को जांचें जो आपको समर्थित फॉर