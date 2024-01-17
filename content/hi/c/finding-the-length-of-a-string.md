---
title:                "स्ट्रिंग की लंबाई का पता लगाना"
html_title:           "C: स्ट्रिंग की लंबाई का पता लगाना"
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## इसक्त्क़ी और क्यों?
एक स्ट्रिंग की लंबाई ढूंढना एक आम कार्य है जो यूजर्स की इनपुट स्ट्रिंग के लंबाई को समझने में मदद करता है। प्रोग्रामर्स स्ट्रिंग की लंबाई को पता करने के लिए उनके कोड में मशीन को बात समझने में मदद करते हैं।

## कैसे करें:
```C
int string_length(char string[]) {
  int length = 0;

  while (string[length] != '\0') {
    length++;
  }

  return length;
}

// उदाहरण:
char str[] = "Hello World";
printf("String length is: %d", string_length(str));

//आउटपुट: String length is: 11
```

## गहराई में खोज करें:
(1) इतिहासिक संदर्भ: C भाषा में स्ट्रिंग लंबाई को पता करने के लिए ```strlen()``` फ़ंक्शन प्रदर्शित करता है जो ANSI C स्टैंडर्ड का हिस्सा है। (2) वैकल्पिक रास्ते: अन्य बहुत सारे भाषाओं में स्ट्रिंग लंबाई को पता करने के लिए विभिन्न लोजिक हो सकते हैं, जैसे कि लूप या फ़ंक्शन इस्तेमाल करके। (3) अंमल का विवरण: स्ट्रिंग लंबाई को बेहतर तरीके से समझने के लिए, हम एक स्ट्रिंग को char एरे के रूप में डेक्लेयर कर सकते हैं। और उसे while लूप द्वारा पास कर सकते हैं जिसमें शर्त ```string[length] != '\0'``` निर्धारित है। 

## अन्य स्रोत देखें:
- [C भाषा सीखें](https://www.learn-c.org/)
- [C भाषा का सरल समरी](https://www.cs.cf.ac.uk/Dave/C/node1.html#SECTION00100000000000000000)