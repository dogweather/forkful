---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/extracting-substrings.md"
---

{{< edit_this_page >}}

Title: C प्रोग्रामिंग में सबस्ट्रिंग निकालना

## क्या और क्यों?
सबस्ट्रिंग निकालने से हमारा मतलब होता है, एक मूल स्ट्रिंग से एक छोटा टुकड़ा पाना। प्रोग्रामर इसे सामग्री को वर्गीकृत करने, खोजने और उसे संशोधित करने के लिए करते हैं।

## कैसे करें:
```C
#include <stdio.h>
#include <string.h>

int main () {
   char str[] = "Hello, world!";
   char sub[40];

   strncpy(sub, str+7, 5);
   sub[5] = '\0';
   
   printf("Extracted substring is: %s\n", sub);
   
   return(0);
}
```
उदाहरण को चलाने पर आपको "world" मिलेगा, जोकि "Hello, world!" का एक सबस्ट्रिंग है।

## गहराई में:
प्रोग्रामिंग के शुरुआती दिनों से ही सबस्ट्रिंग निकालने की आवश्यकता अनुभूत होती आई है। आप इसे `strncpy` के अलावा `memmove`, `strndup` जैसे फ़ंक्शन का उपयोग करके भी कर सकते हैं। `strncpy` इंटर्नली `memcpy` का उपऔग करता है, जो बाइट्स को सीधे मेमोरी में कॉपी करता है।

## और देखें:
1. [C String Manipulation](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
2. [C String Functions](https://www.cplusplus.com/reference/cstring/)