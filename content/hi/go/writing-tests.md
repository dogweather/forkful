---
title:                "टेस्ट लिखना"
html_title:           "Go: टेस्ट लिखना"
simple_title:         "टेस्ट लिखना"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों 

गो प्रोग्रामिंग भाषा में टेस्ट लिखना क्यों जरूरी है? प्रोग्राम डिवेलपमेंट की प्रक्रिया में टेस्ट का एक महत्वपूर्ण भूमिका होता है। टेस्ट लिखने से आपको अपने कोड की गुणवत्ता को सुनिश्चित करने में मदद मिलती है और सुरक्षित और स्थिर कोड तैयार करने में मदद मिलती है।

## कैसे करें

यहां हम आपको गो भाषा में टेस्ट लिखने के लिए कुछ उदाहरण और उनके आउटपुट के साथ "```Go ... ```" कोड ब्लॉक्स के माध्यम से बताएंगे।

उदाहरण १:
```Go
package main

import "testing"

func Sum(a, b int) int {
	return a + b
}

func TestSum(t *testing.T) {
	total := Sum(5, 7)
	if total != 12 {
		t.Errorf("Sum(5, 7) was incorrect, got: %d, want: %d.", total, 12)
	}
}
```

उपरोक्त उदाहरण में, हमने एक `Sum()` फ़ंक्शन बनाया है जो दो संख्याओं को जोड़ता है और हमने इसे टेस्ट करने के लिए `TestSum()` टेस्ट फ़ंक्शन बनाया है। हमने एक `if` स्टेटमेंट के माध्यम से टेस्ट किया है कि क्या हमारे द्वारा जोड़े गए संख्याओं का योग सही है या नहीं। अगर गलती होती है, तो हम `t.Errorf()` के माध्यम से एरर प्रिंट करते हैं।

उदाहरण २:
```Go
package main

import "testing"

func Multiply(a, b int) int {
	return a * b
}

func TestMultiply(t *testing.T) {
	result := Multiply(4, 6)
	if result != 24 {
		t.Errorf("Multiply(4, 6) was incorrect, got: %d, want: %d.", result, 24)
	}
}
```

इस उदाहरण में, हमने `Multiply()` फ़ंक्शन बनाया है जो दो संख्याओं को गुणा करता है और हमने इसे टेस्ट करने के लिए `TestMultiply()` टेस्ट फ़ंक्शन बनाया ह