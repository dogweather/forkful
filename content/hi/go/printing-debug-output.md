---
title:                "Go: डीबग आउटपुट को छापना"
simple_title:         "डीबग आउटपुट को छापना"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/printing-debug-output.md"
---

{{< edit_this_page >}}

# क्यों

डिबग आउटपुट प्रिंट करने में शामिल होने का कारण यह है कि यह आपको अपने कोड में कोई त्रुटियां खोजने में मदद करता है और उन्हें ठीक करने में मदद करता है।

# कैसे करें

आप अपने कोड में डिबग आउटपुट प्रिंट करने के लिए `fmt.Println()` फ़ंक्शन का उपयोग कर सकते हैं। नीचे दिए गए कोड ब्लॉक में आपको इसका एक उदाहरण दिया गया है:

```Go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
```

इस कोड को कंपाइल और चलाते हुए आप नीचे दिए गए आउटपुट को देख सकते हैं:

```
Hello, World!
```

# गहराई में

डिबग आउटपुट प्रिंट करने में गहराई में, इसके अन्य विकल्पों के बारे में अधिक जानकारी दी गई है जो आपको अपने काम को आसान और अधिक सुविधाजनक बना सकते हैं। आप `fmt.Printf()` फ़ंक्शन का उपयोग अपने वैरिएबल के फॉर्मैट को जानने के लिए कर सकते हैं या `log` पैकेज का इस्तेमाल करके अपने लोग फ़ाइल में डिबग स्टेटमेंट प्रिंट कर सकते हैं। इन अन्य विकल्पों के बारे में और अधिक जानने के लिए, [गो दस्तावेज़ीकरण](https://golang.org/doc/) पर जाएं।

# देखें भी

- [गो दस्तावेज़ीकरण](https://golang.org/doc/)
- [गो छोड़ (Go Playground)](https://play.golang.org/)
- [गो स्रोत कोड देखने का विधि (Visual Studio Code)](https://code.visualstudio.com/docs/languages/go)