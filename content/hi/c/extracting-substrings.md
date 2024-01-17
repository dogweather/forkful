---
title:                "सबस्ट्रिंग निकालना"
html_title:           "C: सबस्ट्रिंग निकालना"
simple_title:         "सबस्ट्रिंग निकालना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
उभरते हुए स्ट्रिंग्स को अलग निकालने का मतलब टेक्स्ट से अनुभव कुछ हटाना है। कई प्रोग्रामर इसे वैयक्तिक रूप से जानकारी के अंदर संचित रखने के लिए किया जाता है।

## कैसे करें:
इसके लिए, हम कोड में substr() फ़ंक्शन का करना होगा। यह फ़ंक्शन तीन आईसीएसआई इस्तेमाल करके स्ट्रिंग के हिस्सों को बाहर निकलता है। नीचे दिए गए उदाहरण में, हमने “Hello World” स्ट्रिंग से पहले और अंतिम चार हर्फ़ निकाले हैं।

```C
#include <stdio.h>

// substr() फ़ंक्शन का प्रोटोटाइप
char *substr(char *str, int begin, int len);

// व्यापार में उपयोग करने के लिए उदाहरण
int main()
{
    // इस फ़ंक्शन को प्रोग्राम में शामिल करें
    char *string = "Hello World";
    printf("पहला व्याप्ति: %s\n", substr(string, 0, 5)); // प्रिंट्स “Hello”
    printf("अंतिम व्याप्ति: %s", substr(string, 8, 4)); // प्रिंट्स “orld”
    return 0;
}

// substr() फ़ंक्शन का व्यापार टेस्ट के लिए
char *substr(char *str, int begin, int len)
{
    char *substr = malloc(len + 1);
    for (int i = 0; i < len; i++)
    {
        substr[i] = *(str + begin + i);
    }
    substr[len] = '\0';
    return substr;
}
```

तथ्य सूचना: हरफं को फ़िक्सेड जानकारी न केवल में आसान रूप से मिस्त्रील रखने के लिये ह्रसवैस्क। वे हर व्याप्त छोटी ऊपर भरसक में पर CIA फंक्शन को सेंट में हैं।

## देप दाइव:
इस प्रकार से स्ट्रिंग्स से सम्बंधित समसामयिक तथ्य प्रदान किया गया है कि उपयोगकर्ताओं को तकनीकी रूप से विश्लेषण की आवश्यकता है। मार्केट में substr() के लिए कई अलग के अनुबंध उपलब्ध हैं जो समझदार के मुकाम एज करती है साथ ही साथ उपनिपत्ताओं से संभंधित विवरण प्रदान करते हैं। यदि आप स्ट्रिंग्स के अन्य तंत्र के बारे में जानना चाहते हैं तो, आप अपने पसंत के इंज क्रिम्स से संपर्क कर सकते हैं।

## अतिरिक्त स्रोत देखें:
- [C substr() function - GeeksforGeeks](https://www.geeksforgeeks.org/c-strrev-function/)
- [String Manipulation in C - TutorialsPoint](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [CIA Substr() Documentation](https://www.cplusplus.com/reference/cstring/substr/)