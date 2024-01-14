---
title:                "Go: एक टेक्स्ट फाइल पढ़ना"
simple_title:         "एक टेक्स्ट फाइल पढ़ना"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## प्रयोजन

एक पाठ फाइल को पढ़ने का *क्यों* किसी को सम्बन्धित हो सकता है, यह समझने के लिए अहम है कि क्या आपको किसी डेटा, टेक्स्ट संग्रह या संरचना के साथ काम करना है।

## कैसे करें

"```Go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    // फाइल पढ़ने के लिए पथ और नाम दर्ज करें
    data, err := ioutil.ReadFile("sample.txt")
    if err != nil {
        fmt.Println("फाइल खोलने में समस्या हुई:", err)
    }

    // पढ़ा गया डेटा प्रिंट करें
    fmt.Println(string(data))
}
```
आउटपुट:
```
यह एक नमूना लेख है जो पाठ फ़ाइल से पढ़ा गया है।
```

## गहराई खोज करें

जब आप किसी भी प्रकार की फाइल से पढ़ाई करते हैं, आपको रूपांतरण, विशेष वर्णन और फाइल के अन्य भागों के साथ काम करने के लिए कई विकल्प हो सकते हैं। Go में कैसे इस्तेमाल करने के प्रकार कई हैं, जिसमें `ioutil` या `bufio` के उपयोग से `File` वस्तु को पढ़कर संसाधित करना शामिल है। आप डेटा को बीच में आसानी से विभाजित और पूर्ण फाइल संरचना को साफ़ कर सकते हैं।

## देखें भी

- [गॉ भाषा ट्यूटोरियल](https://golang.org/doc/tutorial/)
- [Go फ़ाइल इ/ओ](https://golang.org/pkg/io/ioutil/)
- [फाइल पढ़ने और लिखने के साथ काम करना](https://gobyexample.com/reading-files)