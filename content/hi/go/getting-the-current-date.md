---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वर्तमान तारीख प्राप्त करना का तरीका सोचना सीधा सा लगता है, लेकिन यह कई प्रोग्राम को बेहतर बनाने के लिए अत्यंत महत्वपूर्ण हो सकता है। उदाहरण के लिए, लॉग फाइलों का समयांकन, प्रयोगकर्ता की गतिविधियों का ट्रेकिंग या किसी ईवेंट को ट्रिगर करने के लिए।

## कैसे करें:

यहां गो लैंग्वेज में वर्तमान तारीख प्राप्त करने का उदाहरण है:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
    currentTime := time.Now()
    fmt.Println("Current Time in Go:", currentTime)
}
```

जब आप इस प्रोग्राम को चलाते हैं, आपको वर्तमान तारीख और समय के साथ एक लाइन मिलेगी।

## गहरी डाइव:

गो में वर्तमान तारीख प्राप्त करने के कई तरीके हैं, लेकिन `time.Now()` सबसे आम तरीका है। इसे डेज़ाइन किया गया है ताकि यह विभिन्न वातावरणों और समय क्षेत्रों में ठीक से काम कर सके।

आप या तो `time` पैकेज का उपयोग कर सकते हैं जो सीधे गो में बिल्ट-इन है या आप अन्य पैकेज, जैसे `jinzhu/now` का उपयोग कर सकते हैं जो कुछ अधिक विविधता प्रदान करते हैं।

## यह भी देखें:

* [Go time package documentation (गो समय पैकेज का डॉक्यूमेंटेशन)](https://pkg.go.dev/time)
* [jinzhu/now package on GitHub (जिनज़ु / अब पैकेज GitHub पर)](https://github.com/jinzhu/now)
* [Dealing with times and dates in Go, on Stack Overflow (स्टैक ओवरफ़्लो पर गो में समय और तारीख के साथ सीन यात्रा)](https://stackoverflow.com/questions/22852781/how-to-handle-date-time-format-with-zone-in-golang)