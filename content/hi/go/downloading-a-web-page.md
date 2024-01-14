---
title:                "Go: वेब पेज डाउनलोड करना"
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों

वेब पेज को डाउनलोड करने का मुख्य कारण है कि आप आप आपकी पसंद की जानकारी को विभिन्न प्रकार के साधनों के माध्यम से एक स्थान से एक स्थान पर आसानी से पहुंच सकते हैं।

## कैसे करें

इसके लिए आपको गो कोड का उपयोग करना होगा। नीचे दिए गए कोड ब्लॉक में आप विभिन्न तरीकों से वेब पेज को डाउनलोड कर सकते हैं।

```Go
package main

import (
  "fmt"
  "io/ioutil"
  "net/http"
)

func main() {
  // वेब पेज को डाउनलोड करने के लिए इस्तेमाल किया जा सकता है
  response, err := http.Get("https://www.example.com")
  if err != nil {
    fmt.Println("डाउनलोड करने में त्रुटि हुई।")
  }
  defer response.Body.Close()
  
  // जानकारी को स्ट्रिंग रूप में अद्यतन करने के लिए इस्तेमाल किया जा सकता है
  body, err := ioutil.ReadAll(response.Body)
  if err != nil {
    fmt.Println("पाठ विश्लेषण में त्रुटि हुई।")
  }
  fmt.Println(string(body))
}
```

## गहराई में

वेब पेज को डाउनलोड करने के और भी कई तरीके हैं, जैसे कि प्रोटोकॉल, कुकीज़, पथ प्राप्त करने आदि। आप इन तरीकों को समझने और अपने कोड में आधार बनाने के लिए गहराई में जानकारी प्राप्त कर सकते हैं।

## देखें भी

- [गो कोड का डॉक्यूमेंटेशन](https://golang.org/doc/)
- [वेब पेज को डाउनलोड करने के लिए गो पैकेज](https://godoc.org/golang.org/x/net/html)
- [गो प्रोग्रामिंग सीखने के लिए लिखित ट्यूटोरियल](https://gobyexample.com/)