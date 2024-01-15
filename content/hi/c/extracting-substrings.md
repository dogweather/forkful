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

## क्यों
कभी-कभी हमें एक बड़ी स्ट्रिंग से छोटी स्ट्रिंग निकालने की आवश्यकता पड़ती है, जैसे कि नाम, ईमेल आईडी या पते को पदों के बीच से अलग करने के लिए। इसलिए हम अपने C कोड में substrings निकालना सीखना चाहेंगे। 

## कैसे करें
```C
// प्रथमीकरण के साथ फ़ंक्शन को बनाएं
char *substr(char *src, int start, int end)
{
    int len = end - start;
    
    // मेमोरी आवंटन करें
    char *result = (char *)malloc(sizeof(char) * len + 1);

    // स्ट्रिंग से प्रतिलिपि बनाएं
    strncpy(result, src + start, len);

    // NULL एंडिंग को जोड़ें
    result[len] = '\0';

    // प्रतिलिपि वापस करें
    return result;
}

// उदाहरण के लिए स्ट्रिंग बनाएं
char *name = "John Smith";

// स्ट्रिंग से स्ट्रिंग निकालें
char *last_name = substr(name, 5, 10);

// कंसोल पर प्रिंट करें
printf("%s", last_name);

// आउटपुट: Smith 
```
## गहराई में जाइए
substr() फ़ंक्शन से हम अपने स्ट्रिंग से एक सबस्ट्रिंग निकाल सकते हैं, जो कि वास्तव में स्ट्रिंग के मेमोरी के अंश पर पॉइंट करता है। यहां, हमने मेमोरी को संबोधित करने के लिए ```malloc()``` फ़ंक्शन का उपयोग किया है और उसके बाद स्ट्रिंगों के लिए ```strncpy()``` फ़ंक्शन का उपयोग किया है। यदि आप फ़ंक्शन को बहुत बार उपयोग करना चाहते हैं तो ```free()``` फ़ंक्शन का भी उपयोग करें, जिससे कि मेमोरी लीक न हो। 

## और भी देखें
- C डेरिक्टिव्स गाइड (https://www.geeksforgeeks.org/c-language-2-gq/) 
- श्रृंखला में स्ट्रिंग हैंडलिंग (https://www.hackerrank.com/challenges/playing-with-characters/problem)