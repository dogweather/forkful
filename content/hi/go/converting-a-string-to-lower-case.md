---
title:                "Go: स्ट्रिंग को लोअर केस में रूपांतरण करना"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों 

यदि आप गो प्रोग्रामिंग सीख रहे हैं तो आपने शायद ये सुना होगा कि स्ट्रिंग को छोटा लिखने के लिए मेथड lower का उपयोग किया जाता है। इस ब्लॉग पोस्ट के माध्यम से हम इस प्रक्रिया को क्यों करते हैं और कैसे करते हैं, इसके बारे में जानकारी देंगे।

## कैसे करें 

यदि आपको गो प्रोग्रामिंग का एक्सपीरियंस होता तो आप जानते होंगे कि स्ट्रिंग को छोटा करने के लिए मेथड lower का उपयोग कैसे किया जाता है। लेकिन यदि आप नए हैं और कैसे करें, तो यह कोड ब्लॉक आपको मदद करेगा:

```Go
package main

import "fmt"
import "strings"

func main() {
	str := "HELLO, WORLD!"
	fmt.Println(strings.ToLower(str))
}
```

आउटपुट:

```Go
hello, world!
```

जैसा कि आप देख सकते हैं, हमने स्ट्रिंग को छोटा किया है और स्थानिक कनवेंशन के अनुसार लिखा है। इस तरह से कोड को और अधिक साफ और समझने में आसान बना सकते हैं।

## गहरी खुदाई 

अब जब हमने जान लिया है कि कैसे स्ट्रिंग को छोटा करते हैं, हम थोड़ी गहराई से इसके बारे में विस्तार से जान सकते हैं। lower मेथड गो में यूनिकोड कैसे हैंडल करता है, और इसमें कैसे केस और स्पेसिंग की प्रश्नों से सामना किया जाता है। आप इसके बारे में और जानकारी के लिए [Go डॉक्युमेंटेशन](https://golang.org/pkg/strings/#ToTitle) देख सकते हैं।

## देखें भी 

- [The Complete Guide to Strings in Go](https://www.ardanlabs.com/blog/2018/03/strings-go-developer.html)
- [Manipulating Strings in Go: A Practical Guide](https://tutor