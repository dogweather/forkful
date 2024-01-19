---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

'String' को lower case में बदलना मतलब दिए गए text के सभी पात्रों को छोटी अक्षर में परिवर्तित करना होता है। इसे कार्यक्रमकर्ता बिना किसी प्रकार के data inconsistency के उपयोगकर्ता के इनपुट को normalize करने के लिए करते हैं।

## कैसे:

Go में, आप`strings.ToLower()` फ़ंक्शन का उपयोग करके इसे कर सकते हैं। यहाँ एक उदाहरण है:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "HELLO, WORLD!"
	lowerStr := strings.ToLower(str)
	fmt.Println(str)
	fmt.Println(lowerStr)
}
```

जब आप इसे चलाते हैं, आपको निम्नलिखित आउटपुट मिलेगा:

```
HELLO, WORLD!
hello, world!
```

## गहरी डाइव

इतिहास में, कंप्यूटर सिस्टम आमतौर पर case-sensitive थे और अक्सर कोड को normalize करने के लिए इनका इस्तेमाल किया गया। आज भी strings.ToLower() फ़ंक्शन का उपयोग case-insensitive comparison, खोज, और sorting के लिए Back-end में किया जाता है।

Go में, ऐल्टरनेटिव तरीके `bytes.EqualFold()` और `strings.EqualFold()` हो सकते हैं, लेकिन ये methods फ़ंक्शनेलिटी भिन्न हैं क्योंकि वे comparison करते हैं और केवल boolean return करते हैं।

`strings.ToLower()` इसे unicode points को lowercase में बदलकर करता है जो कि international characters के साथ काम करना अधिक सहायक बनाता है।

## देखने के तौर पर 

1. strings package documentation in Go: http://golang.org/pkg/strings/
2. Go Blog - Strings, bytes, runes and characters in Go: https://blog.golang.org/strings