---
title:                "स्ट्रिंग से तारीख निकालना"
html_title:           "C: स्ट्रिंग से तारीख निकालना"
simple_title:         "स्ट्रिंग से तारीख निकालना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डेटा को स्ट्रिंग से पार्स करना एक काफी आम समस्या है जोकि दिनांकों को स्पष्ट रूप से प्रस्तुत करने के लिए किया जाता है। इसका उद्देश्य प्रोग्रामर्स को दिनांक और समय संबंधित काम करने में मदद करना होता है।

## कैसे करें:

```C
#include<stdio.h>
#include<stdlib.h>

int main(){
    char str_date[] = "20/01/2021"; // स्ट्रिंग
    int day, month, year; // इंटीजर वेरिएबल्स डेट और समय को होल्ड करने के लिए
    
    sscanf(str_date, "%d/%d/%d", &day, &month, &year); // स्ट्रिंग से डेट को पार्स करें

    printf("दिनांक: %d/%d/%d", day, month, year); // आउटपुट: दिनांक: 20/01/2021
    
    return 0;
}
```

## गहराई से जानें:

पहले से ही डेट को स्ट्रिंग से पार्स करने के विभिन्न तरीके मौजूद हैं, जैसे वेरिएबल्स को स्प्लिट करके खुद से पार्स करना या तो टोकनाइजर का उपयोग करना। लेकिन C में यह प्रचलित तरीका है। स्टैंडर्ड लाइब्रेरी से पार्स करने के लिए अन्य फ़ंक्शन भी उपलब्ध हैं। साथ ही इसका उपयोग डेट को फॉर्मेट करने या इसे अन्य तरीकों से संसाधित करने में भी किया जा सकता है। 

## इसे भी देखें:

स्ट्रिंग से डेट को पार्स करने की और विस्तृत जानकारी के लिए निम्नलिखित लिंक देखें:

- [स्टेकओवरफ्लो पोस्ट](https://stackoverflow.com/questions/44383417/parse-date-in-c-from-string) 
- [कार्नल और स्टैंडर्ड लाइब्रेरी डेट और टाइम फ़ंक्शन्स](https://www.gnu.org/software/libc/manual/html_node/Date-and-Time-Functions.html) 
- [विभिन्न डेट और टाइम स्ट्रिंग फॉर्मेट](https://man7.org/linux/man-pages/man3/strftime.3.html)