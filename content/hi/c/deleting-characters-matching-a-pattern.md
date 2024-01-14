---
title:    "C: पैटर्न के अनुसार अक्षरों को हटाना"
keywords: ["C"]
---

{{< edit_this_page >}}

## क्यों

किसी भी पैटर्न से मिलता जुलता अक्षरों को हटाने के लिए आप क्यों इस काम को करना चाहेंगे, उसके बारे में सिर्फ 1-2 वाक्यों में बताएं।

## कैसे करें

कोडिंग उदाहरण और "```C ... ```" कोड ब्लॉक में नमूना आउटपुट कैसे प्राप्त करें।

```C
#include <stdio.h>

int main() {
    char string[] = "Hello World";
    char pattern = 'l';

    // पैटर्न के साथ मिलते जुलते अक्षरों को हटाने के लिए लूप का उपयोग करें
    for (int i = 0; string[i] != '\0'; i++) {
        if (string[i] == pattern) {
            // हटाएं या छोड़ें
            string[i] = ' ';
        }
    }

    // संशोधित स्ट्रिंग को मुद्रित करें
    printf("बाद में पैटर्न के बिना स्ट्रिंग: %s", string);

    return 0;
}
```

आउटपुट:

```
बाद में पैटर्न के बिना स्ट्रिंग: He o Wor d
```

## गहराई में

पैटर्न से मिलता जुलता अक्षरों को हटाने का और अधिक गहरा ज्ञान। कई भाषाओं में उपलब्ध यह फ़ंक्शनालिटी C में ईम्प्लीमेंट कैसे किया गया है और और इसके अलावा उपलब्ध विकल्पों और विशेषताओं के बारे में हम गहराई करेंगे।

## आगे देखें

[FreeCodeCamp: C Programming](https://www.freecodecamp.org/learn/c-programming/)\
[W3Schools: C String Manipulation](https://www.w3schools.in/c-tutorial/string-manipulation/)\
[Coding Blocks: C Strings and String Manipulation](https://codingblocks.com/resources/c-strings-and-string-manipulation/)