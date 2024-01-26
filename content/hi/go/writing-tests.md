---
title:                "परीक्षण लिखना"
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/writing-tests.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
टेस्ट्स लिखना कोड के छोटे हिस्से को अलग से चला कर ये जांचना है कि वह उम्मीद के मुताबिक काम कर रहे हैं या नहीं। प्रोग्रामर्स इसे बग्स को पहचानने, कोड की क्वालिटी बढ़ाने और भविष्य में गलतियों को रोकने के लिए करते हैं।

## कैसे करें? (How to:)
```Go
package main

import (
    "testing"
)

// Simple function to add two integers
func Add(a int, b int) int {
    return a + b
}

// Test function for Add
func TestAdd(t *testing.testing.T) {
    total := Add(5, 5)
    if total != 10 {
       t.Errorf("Add was incorrect, got: %d, want: %d.", total, 10)
    }
}
```

जब आप ऊपर दिए गए टेस्ट को `go test` कमांड के साथ चलाते हैं, तो निम्नलिखित आउटपुट मिलेगा:
```
PASS
ok      your/package/name 0.002s
```

## गहराई से जानकारी (Deep Dive)
टेस्टिंग में Go का जन्म TDD (Test-Driven Development) कल्चर से हुआ है जो कि सॉफ्टवेयर डेवलपमेंट में बहुत प्रचलित है। `testing` पैकेज Go का मूल औजार है जिससे टेस्ट केस लिखे और चलाए जा सकते हैं। अल्टरनेटिव्स जैसे `Testify` या `GoConvey` भी मौजूद हैं, जो कि अधिक सुविधाओं को प्रदान करते हैं। टेस्ट्स को स्ट्रक्चर करते वक़्त benchmarks और examples भी जोड़े जा सकते हैं।

## और भी सूत्र (See Also)
- Go के ऑफिसियल डाक्यूमेंटेशन में टेस्ट पैकेज [Go Testing Package](https://golang.org/pkg/testing/)
- टेस्ट ड्राइवन डेवलपमेंट पर [Test-Driven Development](https://en.wikipedia.org/wiki/Test-driven_development)
- टेस्टिंग फ्रेमवर्क [Testify on GitHub](https://github.com/stretchr/testify)
- इंटरैक्टिव टेस्टिंग टूल [GoConvey on GitHub](https://github.com/smartystreets/goconvey)
