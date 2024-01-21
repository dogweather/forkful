---
title:                "तारीख को स्ट्रिंग में बदलना"
date:                  2024-01-20T17:36:17.546043-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डेट को स्ट्रिंग में बदलना मतलब होता है तारीख को टेक्स्ट फॉर्मेट में दिखाना। प्रोग्रामर्स ऐसा इसलिए करते हैं ताकि दिनांकों को यूजर-फ्रेंडली फॉर्मेट में प्रिंट या सेव कर सकें। 

## How to: (कैसे करें:)
```C
#include <stdio.h>
#include <time.h>

int main() {
    // वर्तमान समय प्राप्त करें
    time_t now = time(NULL);
    
    // टाइम स्ट्रक्चर में कन्वर्ट करें
    struct tm *ptm = localtime(&now);
    
    // स्ट्रिंग में फॉर्मेट करने का बफर
    char date_str[100];
    
    // स्ट्रिंग में तारीख फॉर्मेट करें
    strftime(date_str, sizeof(date_str), "%d-%m-%Y %H:%M:%S", ptm);
    
    // परिणाम प्रिंट करें
    printf("वर्तमान तारीख और समय: %s\n", date_str);
    
    return 0;
}
```
सैंपल आउटपुट:
```
वर्तमान तारीख और समय: 28-03-2023 14:50:32
```

## Deep Dive (गहराई से जानना):
सालों से, C प्रोग्रामर्स `<time.h>` लाइब्रेरी का इस्तेमाल करते आए हैं। `strftime` फंक्शन समय और तारीख को विभिन्न फॉर्मेट्स में बदलने का एक बहुत ही शक्तिशाली तरीका है। अलग-अलग कल्चर और स्थानों के हिसाब से फॉर्मेट्स बदलते रहते हैं। इस फंक्शन में दर्जनों फॉर्मेट स्पेसिफायर्स होते हैं जिन्हें आप अपनी जरूरत के हिसाब से सेट कर सकते हैं। 

वैसे तो हम strftime का उपयोग कर रहे हैं, लेकिन विभिन्न लाइब्रेरीज और एपीआई में अलग-अलग तारीख-समय से संबंधित फंक्शंस उपलब्ध हैं। उदाहरण के लिए, POSIX मानक में `gmtime` और `localtime` जैसे फंक्शन्स हैं। ऐसे कार्यक्षमताओं का सही चयन करने से प्रोग्राम की गुणवत्ता और कार्यक्षमता बढ़ती है।

## See Also (और भी देखें):
- ISO C और POSIX समय स्टैण्डर्ड्स: [Link to ISO C and POSIX time standards]
- स्ट्रिंग प्रोसेसिंग और मैनीपुलेशन फंक्शन्स: [Link to C string handling functions documentation]
- स्ट्रिंग फॉर्मेट स्पेसिफायर्स: [Link to strftime format specifiers list documentation]