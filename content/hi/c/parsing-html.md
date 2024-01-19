---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML पार्सिंग एक प्रक्रिया है जिसमें HTML कोड को विश्लेषित और अभिप्रेत संरचना में परिवर्तित किया जाता है। प्रोग्रामर्स इसे तख्ती के ऊपर HTML संदेश को प्रकाशित करने के लिए करते हैं।

## कैसे करें:
यहाँ एक HTML पार्सिंग करने का उदाहरण है। 

```C
#include <stdio.h>
#include <stdlib.h>
#include "myhtml/api.h"

int main(int argc, const char *argv[])
{
    char html[24] = "<div><span>Hi there!</span></div>";
    // create new collection
    myhtml_t* myhtml = myhtml_create();
    myhtml_init(myhtml, MyHTML_OPTIONS_DEFAULT, 1, 0);
  
    // parse HTML
    myhtml_parse(myhtml, MyHTML_ENCODING_UTF_8, html, strlen(html));
  
    // done
    myhtml_destroy(myhtml);
    return 0;
}
```

## गहरी डाइव
HTML पार्सिंग के आदान-प्रदान का इतिहास वेब ब्राउज़र की उत्पत्ति से शुरु हुआ है। विकल्पों के तौर पर, वेब स्क्रेपिंग और साहित्यिक विश्लेषण का उपयोग किया जा सकता है, लेकिन वे इतने संवेदनशील नहीं होते हैं। पार्सिंग HTML के लिए लाइब्रेरियों का उपयोग करने का फायदा यह है कि आपको HTML की सूक्ष्म जानकारी के लिए चिंता करने की आवश्यकता नहीं होती।

## अधिक जानने के लिए
1. [HTML टूटोरियल by W3Schools](https://www.w3schools.com/html/)
2. [C प्रोग्रामिंग भाषा ट्यूटोरियल](https://www.learn-c.org/)
3. [HTML parsing libraries in C](https://www.google.com/search?q=HTML+parsing+libraries+in+C)