---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
String का पहला अक्षर बड़ा (capita) करने का मतलब है कि हम string के प्रारंभ में आने वाले अक्षर को uppercase में परिवर्तित करें। Programmers अक्सर user interface में consistency लाने या proper nouns को highlight करने के लिए इसका इस्तेमाल करते हैं।

## How to: (कैसे करें:)
```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	originalString := "नमस्ते दुनिया"
	capitalizedString := strings.Title(strings.ToLower(originalString))
	fmt.Println(capitalizedString) // नमस्ते दुनिया
}
```
Sample Output:
```
नमस्ते दुनिया
```
ध्यान रहे कि `strings.Title()` हर शब्द के पहले अक्षर को capital करता है। अगर आपको सिर्फ पहला अक्षर बड़ा करना है, तो अलग फंक्शन बनाना पड़ेगा।

## Deep Dive (गहराई में जानकारी)
String को capitalize करने की क्रिया सिंपल है, परंतु इसका विकास टाइपराइटर के युग से हुआ है जहां capital letters का उपयोग important text को बताने के लिए होता था।

Go में `strings` पैकेज से `Title` फंक्शन title casing प्रदान करता है, जो हर शब्द के पहले अक्षर को capital बनाता है। UTF-8 support की वजह से, यह सभी languages के लिए काम करता है जिसमें special characters और accents होते हैं। फिर भी, कई बार locale-specific rules होते हैं; इसलिए और भी libraries जैसे कि `golang.org/x/text` का उपयोग बेहतर हो सकता है।  

एक और महत्वपूर्ण बिंदु यह है कि `Title` function विशेष रूप से Buddha को "Buddha" की तरह capitalize करता है, न कि "BUDDHA" के रूप में। आपके कोड में ऐसे संवेदनशील मामलों के लिए अतिरिक्त logic शामिल करना पड़ सकता है। 

## See Also (और भी जानकारी)
- Go documentation for strings package: https://pkg.go.dev/strings
- Go by Example: String Functions: https://gobyexample.com/strings
- `golang.org/x/text`: https://pkg.go.dev/golang.org/x/text
