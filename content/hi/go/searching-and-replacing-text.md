---
title:                "पाठ खोजना और बदलना"
date:                  2024-01-20T17:58:04.838541-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेक्स्ट सर्चिंग और रिप्लेसिंग का मतलब है किसी दस्तावेज़ में शब्दों या वाक्यांशों को ढूँढना और बदलना। प्रोग्रामर्स इसे डेटा मोडिफिकेशन, एरर करेक्शन, या कोड रिफैक्टरिंग के लिए करते हैं।

## How to: (कैसे करें:)
Go में `strings` पैकेज string manipulation के लिए उपयोगी फंक्शंस provide करता है। ये एक सिम्पल उदाहरण है:

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	originalText := "हेलो, यह एक टेस्ट है।"
	searchFor := "टेस्ट"
	replaceWith := "डेमो"

	// Replace text
	result := strings.Replace(originalText, searchFor, replaceWith, -1)

	fmt.Println("पहले: ", originalText)
	fmt.Println("बाद में: ", result)
}
```

आउटपुट:

```
पहले: हेलो, यह एक टेस्ट है।
बाद में: हेलो, यह एक डेमो है।
```

## Deep Dive (गहराई में जानकारी):
स्ट्रिंग सर्च और रिप्लेसमेंट बेसिक प्रोग्रामिंग में से एक हैं। मानव सभ्यता में लंबे समय से हाथ से लिखे गए टेक्स्ट को एडिट करना रिवाज रहा है, पर डिजिटल युग में यह सरल हो गया है। इसका उपयोग लार्ज फाइल्स में वर्ड्स को जल्दी से ढूंढने और बदलने में होता है। `strings.Replace` हमें गो में यह काम करने की क्षमता देता है, और `-1` third पैरामीटर के रूप में सभी occurrences को रिप्लेस करने का संकेत देता है। वैकल्पिक तरीकों में `Regexp` पैकेज के साथ regular expressions का उपयोग शामिल है जो ज्यादा कॉम्प्लेक्स पैटर्न सर्चिंग में मदद करता है।

## See Also (देखें भी):
- Go स्टैंडर्ड लाइब्रेरी के `strings` पैकेज का डॉक्युमेंटेशन: [strings package](https://pkg.go.dev/strings)
- Go by Example पर स्ट्रिंग फंक्शन गाइड: [Go by Example: String Functions](https://gobyexample.com/string-functions)
- Regular expressions in Go के लिए `regexp` पैकेज का उपयोग: [regexp package](https://pkg.go.dev/regexp)
