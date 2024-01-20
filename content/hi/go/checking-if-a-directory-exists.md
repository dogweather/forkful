---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:57:21.300599-07:00
html_title:           "Elm: डायरेक्टरी का अस्तित्व जाँचना"
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डायरेक्टरी का अस्तित्व जांचना कहते हैं कि हम पता करें कि कोई फोल्डर कंप्यूटर में है या नहीं। प्रोग्रामर्स इसकी जांच इसलिए करते हैं क्योंकि फाइल ऑपरेशन्स से पहले यह जरूरी होता है कि हमें पता हो डायरेक्टरी मौजूद है या नहीं।

## How to: (कैसे करें:)
Go में डायरेक्टरी के होने का पता लगाने के लिए `os` पैकेज का उपयोग करें। 

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	dir := "/path/to/directory"

	// Stat फंक्शन हमें फाइल या डायरेक्टरी की जानकारी देगा
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		fmt.Printf("डायरेक्टरी '%s' मौजूद नहीं है.\n", dir)
	} else {
		fmt.Printf("डायरेक्टरी '%s' मौजूद है.\n", dir)
	}
}
```

आउटपुट:
```
डायरेक्टरी '/path/to/directory' मौजूद है.
- या -
डायरेक्टरी '/path/to/directory' मौजूद नहीं है.
```

## Deep Dive (गहन जानकारी):
जब `os.Stat` फंक्शन को कोई फाइल या डायरेक्टरी नहीं मिलती, तो यह एक एरर लौटाता है। `os.IsNotExist(err)` हमें यह बताता है कि अगर एरर इसलिए है क्योंकि फाइल या डायरेक्टरी मौजूद नहीं है, तो true देगा, अन्यथा false। पहले के समय में, प्रोग्रामर्स डायरेक्टरी का पता लगाने के लिए डायरेक्टरी को खोलने की कोशिश करते थे, पर वह पुराना तरीका है और कभी-कभी समस्या पैदा कर सकता है। `os.Stat` एक स्टैंडर्ड और भरोसेमंद तरीका हो गया है।

## See Also (और देखें):
- Go by Example पर फाइल सिस्टम के अध्ययन के लिए: [Go by Example: Directories](https://gobyexample.com/directories)
- The Go Programming Language Specification पर 'os' पैकेज की और जानकारी: [Package os](https://golang.org/pkg/os/)