---
title:                "भविष्य या अतीत में तारीख की गणना"
date:                  2024-01-20T17:31:29.001775-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

तारीख की गणना मतलब आज की तारीख से भविष्य या अतीत की तारीख का पता लगाना। प्रोग्रामर इसे reminders, bookings, और ऐतिहासिक डाटा एनालिसिस के लिए करते हैं।

## कैसे करें? (How to:)

C में तारीख की गणना के लिए `time.h` लाइब्रेरी का उपयोग होता है। आइए देखें:

```c
#include <stdio.h>
#include <time.h>

int main() {
    // वर्तमान तारीख और समय
    time_t now;
    time(&now);
	
    // 10 दिन के लिए सेकंड्स
    const int DAYS = 10;
    const int SECONDS_PER_DAY = 86400;
    
    // 10 दिन बाद की तारीख
    time_t future = now + (DAYS * SECONDS_PER_DAY);

    // तारीख को स्ट्रिंग में परिवर्तित करें
    char str[26];
    ctime_r(&future, str);

    printf("आज की तारीख: %s", ctime(&now));
    printf("10 दिन बाद की तारीख: %s", str);

    return 0;
}
```

सैंपल आउटपुट:
```
आज की तारीख: Mon Jan 1 12:00:00 2023
10 दिन बाद की तारीख: Thu Jan 11 12:00:00 2023
```

## गहराई से जानकारी (Deep Dive)

तारीख की गणना C में `time.h` के जरिए होती है जो 1970 से UNIX समय का उपयोग करती है। आप `mktime` और `localtime` जैसे फंक्शन्स के साथ भी खेल सकते हैं। अल्टरनेटिव में library functions जैसे की `strftime` और `strptime` हैं जो अधिक फ्लेक्सिबल फॉर्मेटिंग और पार्सिंग ऑप्शंस ऑफर करते हैं। इंप्लीमेंटेशन की बात करें तो, और भी चुनौतियां हैं जैसे कि टाइम जोन्स, लीप ईयर्स, और डेलाइट सेविंग्स, जिन्हें सही से हैंडल करना जरूरी है।

## संबंधित जानकारी (See Also)

- [C Date and Time](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm) - और जानें strftime और strptime के बारे में।
- [time.h Reference](https://en.cppreference.com/w/c/chrono) - C में समय की गणना करने वाले फंक्शन्स के रेफरेन्सेस।
- [Date and Time in C](https://en.wikibooks.org/wiki/C_Programming/time.h) - C में दिनांक और समय से संबंधित अधिक जानकारी।
