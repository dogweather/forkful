---
title:                "Go: स्ट्रिंग को मुख्याधिकृत करना"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##क्यों

कोई भी प्रोग्रामर या गोलंग डेवलपर एक डेटा स्ट्रिंग को कैपिटलाइज करना चाहते हैं जब वे उसे अपनी प्रोग्रामिंग लैंग्वेज कोड या स्क्रिप्ट में उपयोग करना चाहते हों।

##कैसे करें

यहां मैं आपको गोलांग में स्ट्रिंग को कैपिटलाइज करने के लिए कुछ सरल कोड दिखाऊंगा:

```Go
package main

import "fmt"
import "strings"

func main() {
	str := "hello world"
	capitalizedStr := strings.ToUpper(str)
	fmt.Println(capitalizedStr)
}
```

आउटपुट: HELLO WORLD

इस उदाहरण में, हमने स्ट्रिंग को "strings" पैकेज के "ToUpper" फ़ंक्शन का उपयोग करके कैपिटलाइज किया है। आप इसे अपने प्रोग्रामिंग लैंग्वेज कोड में भी उपयोग कर सकते हैं।

##डीप डाइव

स्ट्रिंग को कैपिटलाइज करने के लिए गोलांग में "strings" पैकेज में कई अन्य फ़ंक्शन भी मौजूद हैं। आप "Title" फ़ंक्शन का उपयोग करके स्ट्रिंग के प्रथम अक्षर को कैपिटलाइज कर सकते हैं, "ToLower" फ़ंक्शन का उपयोग करके स्ट्रिंग को सामान्य रूप से छोटा कर सकते हैं और "ToUpperSpecial" फ़ंक्शन का उपयोग करके आप किसी अन्य अलग भाषा के लिए स्ट्रिंग को कैपिटलाइज कर सकते हैं।

##देखें भी

- Go स्ट्रिंग्स [डॉक्यूमेंटेशन](https://golang.org/pkg/strings/)
- [Golang विधिंमल्ला: स्ट्रिंग्स का प्रयोग करना](https://www.golang-book.com/books/intro/5)
- [गोलांग का ऑनलाइन प्रोग्रामिंग कार्यक्रम](https://tour.golang.org/welcome/1)