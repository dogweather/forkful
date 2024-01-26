---
title:                "सबस्ट्रिंग्स निकालना"
date:                  2024-01-20T17:46:06.952540-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
'उपस्ट्रिंग' का अर्थ है स्ट्रिंग का एक हिस्सा निकालना। प्रोग्रामर्स डेटा को पार्स करने, खास जानकारी हासिल करने या यूजर्स को विशिष्ट प्रकार का डेटा दिखाने के लिए उपस्ट्रिंग निकालते हैं।

## How to: (कैसे करें)
Go में उपस्ट्रिंग निकालने के लिए `slice` का इस्तेमाल करते हैं। देखें उदाहरण:

```Go
package main

import (
	"fmt"
)

func main() {
	str := "नमस्ते दुनिया"
	substr := str[7:14]
	fmt.Println(substr) // दुनिया
}
```

`substr := str[start:end]` अंदाज़ में `start` इंडेक्स से शुरू करके और `end-1` इंडेक्स पर खत्म करके उपस्ट्रिंग का चयन होता है।

## Deep Dive (गहराई से जानकारी)
Go में स्ट्रिंग्स UTF-8 इन्कोडेड होती हैं, इसलिए उपस्ट्रिंग का तरीका थोड़ा जटिल हो सकता है जब मल्टीबाइट कैरेक्टर शामिल हों। UTF-8 इन्कोडेड स्ट्रिंग्स के साथ काम करते समय, `rune` का इस्तेमाल करें।
वैकल्पिक तरीकों में स्ट्रिंग लाइब्रेरी के `strings` पैकेज का उपयोग शामिल है। 
उदाहरण के लिए, `strings.Contains`, `strings.Index`, और `strings.Join` कार्य हैं जो स्ट्रिंग्स में उपस्ट्रिंग्स को हेंडल करने के लिए उपयोगी होते हैं।

## See Also (और जानकारी के लिए)
- Go डॉक्स में स्ट्रिंग पैकेज: https://pkg.go.dev/strings
- Go ब्लॉग पर स्ट्रिंग्स: https://blog.golang.org/strings
