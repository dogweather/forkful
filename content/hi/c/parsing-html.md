---
title:                "HTML पार्सिंग"
html_title:           "C: HTML पार्सिंग"
simple_title:         "HTML पार्सिंग"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों
विशेष तौर पर वह लोग जो वेब साइट डेवलपमेंट या स्क्रेपिंग से जुड़े हैं, HTML पार्सिंग में दिलचस्पी रखते हैं। HTML दस्तावेज़ में पाठ, चित्र और संरचना को सूचित करता है, इसलिए दूसरे डेटा स्रोतों से जानकारी खींचने के लिए पार्सिंग उपयोगी हो सकता है।

## कैसे करें
हम इस आलेख में HTML पार्सिंग के बारे में हमें संवेदनशील तरीके से प्रोग्रामिंग करना सीखेंगे। यहां हम ````C```` प्रोग्रामिंग भाषा का उपयोग करेंगे।

यहां हम एक साधारण HTML दस्तावेज़ को पार्स करने के लिए कुछ कोड देंगे। पार्स की गई डॉक्यूमेंट के मूल्यांकन को प्रिंट किया जाएगा। इससे हमें यह आसानी से समझ में आएगा कि कौन से HTML टैग और डाटा को कैसे उपयोग किया गया है।

```C
#include <stdio.h>

int main() {
  char html[] = "<html><head><title>Coding Article</title></head><body><h1>HTML Parsing</h1><p>Learn how to parse HTML in C programming.</p></body></html>";

  char *p = html;
  while(*p) {
    if(*p == '<') {
      do {
        printf("%c", *p);
        p++;
      }
      while(*p != '>');
      printf("%c\n", *p);
    }
    else {
      printf("%c", *p);
    }
    p++;
  }
  return 0;
}
```
### आउटपुट:
```html
<html>
<head>
<title>
</title>
</head>
<body>
<h1>
</h1>
<p>
</p>
</body>
</html>
```

## गहराई में जाएँ
HTML पार्सिंग में कई तरीके हैं, हमने यहां सबसे स्पष्ट तरीका दिखाया है। इसमें *शर्टहैंड* और कुछ विलय कार्य भी हैं।

अधिक जानने के लिए निम्न लिंक्स का उस्तेद करें: 

1. HTML पार्सिंग के लिए C का डॉक्यूमेंटेशन: [