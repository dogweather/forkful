---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

स्ट्रिंग की लंबाई ढूंढना यह जानने का तरीका ​​है कि वह कितने अक्षरों का होता है। प्रोग्रामर इसे यह जानने के लिए करते हैं कि उनके पास कितनी जानकारी संग्रह की गई है, और वे इसे कैसे संदर्भित कर सकते हैं।

## कैसे (How to)

Go लैंग्विज में, हम ```len()``` फ़ंक्शन का उपयोग करके स्ट्रिंग की लंबाई ढूंढ सकते हैं।

```Go
package main

import "fmt"

func main() {
    str := "नमस्ते, दुनिया!"  
    fmt.Println(len(str))
}
```

आउटपुट:
```Go
30
```

## गहराी की जांच (Deep Dive)

Go लैंग्विज में, ```len()``` फ़ंक्शन का उपयोग UTF-8 इनकोडेड स्ट्रिंग की लंबाई निर्धारित करने के लिए किया जाता है। यह स्पष्ट रूप से उन चरित्रों की गणना करता है जिनके पास एक या अधिक बाइट हैं। 

वैकल्पिक रूप से, आप ```unicode/utf8.RuneCountInString()``` फ़ंक्शन का उपयोग कर सकते हैं, जो एक स्थिर विधि है और यह साधा कर सकता है कि आपके स्ट्रिंग में कितने रन हैं। 

```Go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "नमस्ते, दुनिया!"  
    fmt.Println(utf8.RuneCountInString(str))
}
```

आउटपुट:
```Go
13
```

## देखें भी (See Also)

[`len` का उपयोग GO दस्तावेज़ीकरण](https://pkg.go.dev/builtin#len)

[`utf8.RuneCountInString` क्लास GO दस्तावेज़ीकरण](https://pkg.go.dev/unicode/utf8#RuneCountInString)