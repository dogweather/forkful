---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
date:                  2024-01-20T17:42:13.769801-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पैटर्न से मेल खाने वाले करैक्टर्स को हटाना मतलब यह है कि किसी स्ट्रिंग से खास करैक्टर्स को इस प्रकार से निकाल देना कि जो हमारे काम के हों वही बचें। प्रोग्रामर्स यह तब करते हैं जब उन्हें डेटा साफ करना होता है या सिर्फ ज़रूरी जानकारी निकालनी होती है।

## How to: (कैसे करें:)
```c
#include <stdio.h>
#include <string.h>

void remove_chars(char *str, const char *chars_to_remove) {
    for (int i = 0, j = 0; str[i] != '\0'; ++i) {
        char *c = strchr(chars_to_remove, str[i]);
        if (c == NULL) {
            str[j++] = str[i];
        }
    }
    str[j] = '\0'; // Null-terminate the modified string
}

int main() {
    char str[] = "Hello, नमस्ते! 123";
    const char *chars = ",!123";

    printf("Original String: %s\n", str);
    remove_chars(str, chars);
    printf("Modified String: %s\n", str);

    return 0;
}
```
Sample Output:
```
Original String: Hello, नमस्ते! 123
Modified String: Hello नमस्ते 
```

## Deep Dive (गहराई से जानकारी):
स्ट्रिंग प्रोसेसिंग और डेटा सेनेटाइजेशन प्राचीन प्रोग्रामिंग समस्याओं में से एक है। स्ट्रिंग्स से विशेष करैक्टर्स को हटाना कई दफा आवश्यक होता है, जैसे लॉग फाइलों से अनचाही जानकारी को निकालना। `strchr` फंक्शन C में इस्तेमाल होता है जो निर्दिष्ट करैक्टर की पहली उपस्थिति को खोजता है। अल्टरनेटिव्स में `strpbrk` या regex लाइब्रेरीज जैसे POSIX या PCRE शामिल हैं, लेकिन वे ज्यादा कॉम्प्लेक्स होते हैं। एक सरल लूप और `strchr` से पता चलता है कि बिना जटिलता के कैसे ऐसा काम किया जा सकता है।

## See Also (इसे भी देखें):
- और विस्तार में स्ट्रिंग हैंडलिंग के लिए [C String Handling](https://www.cplusplus.com/reference/cstring/)
- POSIX रेगुलर एक्सप्रेशंस के लिए [POSIX Regex](https://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap09.html)
- PCRE लाइब्रेरी के उपयोग के लिए [PCRE Library](https://www.pcre.org/)