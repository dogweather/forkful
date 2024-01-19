---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/extracting-substrings.md"
---

{{< edit_this_page >}}

# उप स्ट्रिंग निकालना (Go में)

## क्या और क्यों?
उप स्ट्रिंग (Substring) निकालना एक क्रिया होती है जिसमें हम एक बड़ी स्ट्रिंग से कुछ छोटे टुकड़े काट लेते हैं। प्रोग्रामर्स इसे डाटा संगठन और मैनिप्युलेशन के लिए करते हैं।

## कैसे करें:
यह देखिए कैसे स्ट्रिंग्स को उप-स्ट्रिंग बना सकते हैं Go का उपयोग करके। 

```Go
package main

import "fmt"

func main() {
    str := "namaste duniya"
    substr := str[0:7]
    fmt.Println(substr)
}
```
इसके उत्तर में निम्नलिखित आउटपुट देना चाहिए।

```
namaste
```

## गहरा विचार:
(1) इतिहास: Go भाषा का निर्माण 2007 में Google द्वारा किया गया था, जिन्होंने उप-स्ट्रिंग उत्पन्न करने की क्षमता को मूल है।
(2) विकल्प: अन्य कुछ भाषाओं में, उप-स्ट्रिंग्स को प्राप्त करने की कुछ अलग-अलग विधियाँ हो सकती हैं। आप `substring()`, `slice()`, या `split()` फ़ंक्शन्स का उपयोग करके इसे कर सकते हैं।
(3) क्रियान्वयन विवरण: Go में, उप-स्ट्रिंग्स `slice` सिंटेक्स का उपयोग करके बनाए जाते हैं। 

## देखने के लिए भी:
और अधिक जानकारी के लिए, यहाँ देखें:
1. [Go डॉक्स स्ट्रिंग्स](https://golang.org/pkg/strings/)
2. [Go डॉक्स स्लाइस](https://golang.org/doc/effective_go#slices)
3. [Go गाइड स्ट्रिंग में slice](https://blog.golang.org/strings)