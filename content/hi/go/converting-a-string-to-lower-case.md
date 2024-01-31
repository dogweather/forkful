---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
date:                  2024-01-20T17:39:01.388329-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग को लोअर केस में बदलने का मतलब है सारे अक्षरों को छोटा (lowercase) करना। अक्सर करके प्रोग्रामर यह इसलिए करते हैं ताकि टेक्स्ट की तुलना करते समय कैपिटलाइजेशन गलतियों से बचा जा सके।

## How to: (कैसे करें:)
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	originalString := "Namaste Duniya!"
	lowerCaseString := strings.ToLower(originalString)
	fmt.Println(lowerCaseString) // आउटपुट: namaste duniya!
}
```
इस कोड का प्रयोग करते हुए हम 'strings' पैकेज के 'ToLower' फ़ंक्शन की मदद से स्ट्रिंग को लोअर केस में बदल सकते हैं।

## Deep Dive (गहराई में):
'ToLower' फ़ंक्शन, जो 'strings' पैकेज में होता है, यह Go प्रोग्रामिंग भाषा के सुरुवाती संस्करण से ही उपलब्ध है। इसके अलावा, Go में यूनिकोड/UTF-8 सपोर्ट बहुत अच्छा है, इसलिए 'ToLower' सही ढंग से अंतर्राष्ट्रीय अक्षरों को भी हैंडल करता है।

वैकल्पिक तरीके के तौर पर, कुछ पुराने सिस्टम्स में लोग ASCII मानक के अनुसार केवल अंग्रेजी अक्षरों के लिए एक सीधा लूप चलाते थे। लेकिन यह तरीका यूनिकोड सपोर्ट के अभाव में सीमित था।

जब हम 'ToLower' का इस्तेमाल करते हैं, तो इंटरनली गो का रनटाइम एक तालिका का प्रयोग करता है जो सभी यूनिकोड कैरेक्टर्स को उनके लोअर केस समकक्षों में मैप करता है।

## See Also (और भी जानकारी के लिए):
- Go डॉक्स में 'strings' पैकेज: [https://pkg.go.dev/strings](https://pkg.go.dev/strings)
- Go दस्तावेज़ीकरण में UTF-8 सपोर्ट: [https://blog.golang.org/strings](https://blog.golang.org/strings)
- Unicode के बारे में और इनफार्मेशन: [https://unicode.org](https://unicode.org)
