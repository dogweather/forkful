---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## "क्या और क्यों?" (What & Why?)

"पार्सिंग ए डेट फ्रॉम ए स्ट्रिंग" एक क्रिया होती है जिसमें हम एक तारिख को string से पढ़ते हैं। यह तारीख को इस्तेमाल करने और संगठित करने के लिए प्रोग्रामर्स द्वारा की जाने वाली सामान्य क्रियाओं में से एक है।

## "कैसे" (How to:)

यहां सी भाषा में कोडिंग उदाहरण और उसके आउटपुट दिए गए हैं:

```C
#include<stdio.h>
#include<time.h>

int main() {
    struct tm tm;
    char buf[255];
    char *s = "01/12/2021 10:20";
    memset(&tm, 0, sizeof(struct tm));
    strptime(s, "%d/%m/%Y %H:%M", &tm);
    strftime(buf, sizeof(buf), "%d %B %Y %H:%M", &tm);
    printf("%s\n", buf);
    return 0;
}
```
विभाजकों एवं तारीख/समय के प्रारूप को ध्यान में रखकर उचित परिवर्तन करें। 

आउटपुट होगा:

```
01 December 2021 10:20
```

## "गहराई से जानिए" (Deep Dive)

तारीख पार्स करने के लिए कई तरीके हैं और यह अधिकतर प्रोग्रामिंग भाषाओं में समर्थित है। अधिक कार्यक्षमता और नियंत्रण के लिए, विशेषकृत तारीख/समय लाइब्रेरीज़ का उपयोग करना भी एक विचारणीय विकल्प हो सकता है।

C में, `strptime` और `strftime` फंक्शन का उपयोग करके आसानी से डेट स्ट्रिंग्स को पार्स किया जा सकता है। `strptime` फंक्शन का उपयोग करके, आप स्ट्रिंग में डेट और समय को `struct tm` में पार्स करते हैं। फिर `strftime` फंक्शन का उपयोग करके आप उस को वांछित प्रारूप में परिवर्तित कर सकते हैं। 

## "अन्य उपयोगी संसाधन" (See Also)

* [C Library - <time.h>](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
* [C - Date & Time](https://www.tutorialspoint.com/cprogramming/c_date_time.htm)
* [strftime function](http://www.cplusplus.com/reference/ctime/strftime/)
* [strptime function](https://man7.org/linux/man-pages/man3/strptime.3.html)