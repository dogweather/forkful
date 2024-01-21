---
title:                "सबस्ट्रिंग्स निकालना"
date:                  2024-01-20T17:45:46.428170-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Substring एक स्ट्रिंग का हिस्सा होता है। इसे निकालने की जरूरत प्रोग्रामिंग में तब पड़ती है जब हमें बड़ी स्ट्रिंग में से कोई खास जानकारी चाहिए होती है। यह प्रोसेस इन्फॉर्मेशन प्रोसेसिंग, डेटा वैलिडेशन, या UI डिस्प्ले के लिए जरूरी है।

## How to: (कैसे करें:)
यहाँ C के सिम्पल कोड से समझाते हैं कि substring कैसे निकालते हैं।

```C
#include <stdio.h>
#include <string.h>

void extractSubstring(char* string, int start, int length) {
    char substring[length + 1];
    
    strncpy(substring, string + start, length);
    substring[length] = '\0'; // Null-terminate the substring
    
    printf("Extracted substring: %s\n", substring);
}

int main() {
    char myString[] = "Hello, world!";
    extractSubstring(myString, 7, 5); // यहाँ 'world' हिस्सा निकालेंगे।
    return 0;
}
```

सैंपल आउटपुट:
```
Extracted substring: world
```

## Deep Dive (गहराई से जानकारी):
Substring निकालने के लिए C में कोई डायरेक्ट फंक्शन नहीं होता, हमें मैनुअली `strncpy` या लूप्स का उपयोग करना पड़ता है। ऐतिहासिक रूप से, C डिज़ाइन की गई थी सिम्पलिसिटी के लिए, जिसका अर्थ था कम बिल्ट-इन फंक्शंस और ज्यादा मैनुअल कंट्रोल। 

अल्टरनेटिव में, कुछ थर्ड-पार्टी लाइब्रेरीज जैसे कि `Glib` का `g_strndup` फंक्शन होता है जो इस काम को आसान बना देता है, लेकिन इनका उपयोग सिर्फ जरूरत पड़ने पर ही करें क्योंकि ये कोड को हैवी बना सकते हैं। 

इम्प्लीमेंट करने से पहले हमेशा सुनिश्चित करें कि memory bounds का ध्यान रखा जाए, ताकि buffer overflow जैसी सुरक्षा समस्याएँ ना आएं।

## See Also (और भी देखें):
- C Standard Library documentation: http://www.cplusplus.com/reference/clibrary/
- GNU C Library (glibc) documentation: https://www.gnu.org/software/libc/manual/
- 'The C Programming Language' by Brian W. Kernighan and Dennis M. Ritchie - यह किताब C भाषा पर गहराई से ज्ञान देती है।