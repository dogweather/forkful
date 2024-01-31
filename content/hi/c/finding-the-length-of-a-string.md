---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
date:                  2024-01-20T17:47:17.556350-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग की लंबाई जानना स्ट्रिंग के अक्षरों की संख्या को मापता है। प्रोग्रामर्स इनपुट वैलिडेशन, डेटा संसाधन, और यूआई डिस्प्ले के लिए ऐसा करते हैं।

## How to: (कैसे करें:)
```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "नमस्ते दुनिया";
    int length = strlen(str);
    
    printf("स्ट्रिंग की लंबाई है: %d\n", length);
    return 0;
}
```
सैंपल आउटपुट: `स्ट्रिंग की लंबाई है: 21`

## Deep Dive (गहराई में जानकारी)
पहले, C में स्ट्रिंग को null-terminated अर्रे के रूप में संग्रहित किया जाता था। `strlen` फंक्शन, जो `<string.h>` हेडर फाइल में अवस्थित है, परंपरागत रूप से इस्तेमाल होता आया है स्ट्रिंग की लंबाई नापने के लिए।

विकल्प में, `strlen` के बजाय लूप का इस्तेमाल करके भी लंबाई नापी जा सकती है। इसके अलावा, वाइड कैरेक्टर स्ट्रिंग्स के लिए, `wcslen` फ़ंक्शन का प्रयोग होता है।

यह ज्ञान रखना महत्वपूर्ण है कि `strlen` ASCII और सादे UTF-8 एन्कोडेड स्ट्रिंग्स की सही लंबाई देता है, लेकिन बहुबाइट कैरेक्टर सेट्स के साथ यह सही नहीं हो सकता। मल्टीबाईट कैरेक्टर्स के लिए, `mbstowcs` जैसे फंक्शन उपयोगी होते हैं।

## See Also (और जानें)
1. C Standard Library Documentation: https://en.cppreference.com/w/c/string/byte/strlen
2. Multibyte strings in C: https://en.cppreference.com/w/c/string/multibyte
3. Unicode handling in C: https://unicode.org
