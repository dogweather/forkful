---
title:                "स्ट्रिंग को जोड़ना"
date:                  2024-01-20T17:34:13.007490-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

String concatenation से हम अलग-अलग strings को एक साथ जोड़ सकते हैं। प्रोग्रामर्स इसका उपयोग मुख्य रूप से डेटा को व्यवस्थित करने, मैसेजेस बनाने, और यूजर इंटरफेसेस में शब्दों को जोड़ने के लिए करते हैं।

## How to: (कैसे करें:)

```c
#include <stdio.h>
#include <string.h>

int main() {
    char str1[20] = "नमस्ते ";
    char str2[] = "दुनिया!";
    
    strcat(str1, str2); // string concatenation
    printf("%s\n", str1); // प्रिंट करें जुड़ी हुई स्ट्रिंग

    return 0;
}
```

जब आप ये कोड चलाएंगे तो आउटपुट होगा:
```
नमस्ते दुनिया!
```

## Deep Dive (गहराई से जानकारी:)

String concatenation शुरू से ही C प्रोग्रामिंग का हिस्सा रहा है। `strcat()` फ़ंक्शन स्टैंडर्ड C library (`string.h`) का हिस्सा है और यह सुनिश्चित करता है कि पहली स्ट्रिंग के अंत में दूसरी स्ट्रिंग जुड़ जाए। इसके अल्टरनेटिव में `strncat()` आता है, जो concatenate करने की लंबाई को सीमित कर सकता है। बहुत जरुरी है कि जिस स्ट्रिंग पर आपको concatenate करना है उसमें पर्याप्त जगह हो ताकि वहाँ नया डेटा जुड़ सके और बफर ओवरफ्लो की समस्या न हो। `strcat()` का उपयोग करते समय यह ध्यान रखना होता है।

## See Also (और भी जानकारी:)

- C Standard Library: `string.h` (https://en.cppreference.com/w/c/string/byte)
- `strcat()` Function Reference: (https://www.cplusplus.com/reference/cstring/strcat/)
- `strncat()` Function Reference: (https://www.cplusplus.com/reference/cstring/strncat/)
- Secure Coding in C: Buffer Overflows (https://owasp.org/www-project-secure-coding-practices-quick-reference-guide/migrated_content)