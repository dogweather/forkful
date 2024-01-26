---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
date:                  2024-01-20T17:48:25.209669-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग की लंबाई ज्ञात करना मतलब किसी टेक्स्ट के चरित्रों की संख्या जानना। प्रोग्रामर्स इसे इसलिए करते हैं ताकि वे डेटा संसाधित कर सकें, वैधता की जांच कर सकें, या यूज़र इनपुट के साथ काम कर सकें।

## How to (कैसे करें):
```Go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    // एक साधारण स्ट्रिंग
    simpleString := "नमस्ते"
    fmt.Println("स्ट्रिंग की लंबाई:", len(simpleString))

    // उचित UTF-8 सपोर्ट के साथ
    correctLength := utf8.RuneCountInString(simpleString)
    fmt.Println("सही स्ट्रिंग की लंबाई (UTF-8):", correctLength)
}

// Output:
// स्ट्रिंग की लंबाई: 18
// सही स्ट्रिंग की लंबाई (UTF-8): 6
```

## Deep Dive (गहराई से जानकारी):
स्ट्रिंग्स की लंबाई जानने का काम पुराना है, जब से कंप्यूटर टेक्स्ट संग्रहीत करना शुरू किये। पुराने ज़माने में, ASCII स्टैंडर्ड था, लेकिन आजकल UTF-8 ज्यादा इस्तेमाल होता है, क्योंकि यह वैश्विक चरित्र सेट्स को सपोर्ट करता है। `len` फंक्शन बाईट्स की संख्या बताता है, जो अंग्रेजी जैसी साधारण भाषाओं के लिए सही हो सकता है, पर यूनिकोड चरित्रों के साथ यह सही नहीं होता। इसीलिए `utf8.RuneCountInString` का इस्तेमाल करना बेहतर होता है, क्योंकि यह सही चरित्र संख्या देता है।

## See Also (और जानकारी के लिए):
- Go के डॉक्स में `utf8` पैकेज: [Package utf8](https://golang.org/pkg/unicode/utf8/)
- स्ट्रिंग्स से जुड़े Go ब्लॉग पोस्ट: [Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)
- यूनिकोड और UTF-8 के ऊपर एक गाइड: [Unicode and Go](https://blog.golang.org/normalization)
