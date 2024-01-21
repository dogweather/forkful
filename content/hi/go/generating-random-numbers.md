---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:50:00.636658-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रैंडम नंबर जनरेट करना कंप्यूटर प्रोग्राम में एक अनिश्चितता जोड़ता है। प्रोग्राम में खेल, सुरक्षता और विज्ञान सम्बंधित सिमुलेशन के लिए यह जरूरी है।

## How to: (कैसे करें)
Go में रैंडम नंबर जनरेट करने के लिए `math/rand` लाइब्रेरी का उपयोग किया जाता है।

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// रैंडम जनरेटर को अनिश्चित बनाने के लिए वर्त्तमान समय से बीज(seed) डालें
	rand.Seed(time.Now().UnixNano()) 

	// int के रूप में एक रैंडम नंबर जनरेट करें
	randomNumber := rand.Intn(100) // 0 से 99 के बीच का नंबर
	fmt.Println("रैंडम नंबर:", randomNumber)
}
```

सैंपल आउटपुट:

`रैंडम नंबर: 42`

## Deep Dive (गहराई से जानकारी)
पहले केम्प्यूटर्स में रैंडम नंबर्स बहुत ही जटिल प्रक्रिया के साथ जनरेट किये जाते थे। `math/rand` में जो प्रणाली है, वह वास्तव में 'स्युडोरैंडम' (pseudorandom) है, मतलब दिए हुए बीज(seed) के साथ ये समान श्रृंखला में नंबर फिर से जनरेट कर सकता है। अगर सच में रैंडम नंबर चाहिए तो `crypto/rand` लाइब्रेरी का उपयोग करना होगा।

इसके अलावा, विभिन्न नंबर जनरेटिंग एलगोरिथ्म जैसे कि लिनियर कंगरुएंट जनरेटर (LCG), मर्सेन ट्विस्टर (MT) इत्यादि होते हैं जो विभिन्न उपयोग-केस में प्रयोग होते हैं।

## See Also (और भी देखें)
- Go by Example: Random Numbers: https://gobyexample.com/random-numbers
- The Go Blog - The Go Playground: https://blog.golang.org/playground
- Package rand: https://golang.org/pkg/math/rand/
- Cryptographically secure random number in Go: https://golang.org/pkg/crypto/rand/