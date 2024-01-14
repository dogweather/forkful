---
title:                "Go: स्ट्रिंग को लोअर केस में रूपांतरित करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरित करना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

यदि आप अपने Go प्रोग्राम में स्ट्रिंग को लोअर केस में कन्वर्ट करना चाहते हैं, तो इसके कई फायदे हैं। लोअर केस में स्ट्रिंग को कन्वर्ट करना मदद करेगा कि आपके कोड लेखन में अधिक स्पष्टता हो, स्ट्रिंग की तुलना आसानी से की जा सके और अन्य विभिन्न फ़ंक्शन के साथ स्ट्रिंग का उपयोग करने में भी आसानी हो।

## कैसे

अपनी Go एप्लीकेशन में स्ट्रिंग को लोअर केस में कन्वर्ट करने के लिए, आपको `strings` पैकेज का उपयोग करना होगा। पहले स्ट्रिंग को `ToLower` फ़ंक्शन के साथ पास करें और फिर नतीजे को एक वैरिएबल में स्टोर करें। यहां एक उदाहरण है:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Go Programming"
    lower := strings.ToLower(str)
    fmt.Printf("%s", lower)
}

// आउटपुट: go programming
```

## गहराई से जानें

स्ट्रिंग को लोअर केस में कन्वर्ट करना काफी सरल है, लेकिन इसके पीछे काफी गहराई है। स्ट्रिंग को कन्वर्ट करने के लिए `ToLower` फ़ंक्शन को `strings` पैकेज के साथ व्यवहारिक रूप से कैसे इम्प्लीमेंट किया गया है, इसके बारे में और जानने के लिए आप इन लिंक्स पर जा सकते हैं:

- [Golang.org मैनुअल](https://golang.org/pkg/strings/#ToLower)
- [Go ब्लॉग पोस्ट: देखभाल की गई स्ट्रिंग्स](https://blog.golang.org/strings) 

## देखें भी

- [Go प्रोगामिंग के बारे में और जानिए](https://golang.org/)
- [Golang हिंदी भाषाकोश](https://github.com/golangtranslations/golang-HI)
- [हिंदी में Go प्रोग्रामिंग सीखें](https://www.studytonight.com/go)