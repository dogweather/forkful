---
title:                "C: एक तारीख को स्ट्रिंग में रूपांतरित करना"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कई बार हमें अपने C प्रोग्राम में तारीख को स्ट्रिंग में बदलने की जरूरत होती है। इससे हमें तारीख को आसानी से प्रिंट करने आयात डेटा में भी उपयोग कर सकते हैं।

## कैसे करें

यदि आपको तारीख को स्ट्रिंग में बदलने की जरूरत है, तो निम्न उदाहरण आपको गाइड करेंगे।

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // तारीख को स्ट्रिंग में बदलें
    time_t now = time(NULL);
    struct tm *t = localtime(&now);
   
    char date_string[50];
    strftime(date_string, sizeof(date_string), "आज की तारीख: %d %B %Y", t);
    // आउटपुट: आज की तारीख: 08 मई 2021
   
    return 0;
}
```

## गहराई में जाएं

तारीख को स्ट्रिंग में बदलने के लिए हम strftime फ़ंक्शन का इस्तेमाल करते हैं। इसमें हम दिए गए फ़ॉर्मेट के अनुसार तारीख को स्ट्रिंग में बदलते हैं। स्ट्रिंग विशेषकरताओं का उपयोग करके हम तारीख को और भी बदल सकते हैं। strftime फ़ंक्शन के अपने अन्य प्रारूपों के बारे में अधिक जानने के लिए आप निम्नलिखित लिंक के जरिए इसे चेक कर सकते हैं।

## देखें भी

- [C में तारीख और समय का उपयोग करना](https://www.programiz.com/c-programming/library-function/time)
- [strftime फ़ंक्शन के अन्य प्रारूप](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [विस्तार से पढ़ें strftime फ़ंक्शन](https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#Formatting-Calendar-Time)