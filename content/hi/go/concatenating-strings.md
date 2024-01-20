---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/concatenating-strings.md"
---

{{< edit_this_page >}}

# स्ट्रिंग कंकटिनेशन (String Concatenation) गो (Go) में 

## क्या और क्यों? 

स्ट्रिंग कंकटिनेशन से आप दो या दो से अधिक स्ट्रिंग्स को जोड़ सकते हैं। प्रोग्रामर्स इसे तब करते हैं जब उन्हें विभिन्न स्ट्रिंग्स को एक साथ योजित करना होता है।

## ऐसे करें:

Go का चौंकने वाला कोड हो सकता है:

```Go
package main

import (
    "fmt"
)

func main() {
    hello := "नमस्ते"
    world := "दुनिया"
    message := hello + " " + world

    fmt.Println(message) 
}
```

आउटपुट:

```Shell
नमस्ते दुनिया
```

## गहराई में:

### हिस्टोरिकल कंटेक्स्ट:

Go, जिसे गगल ने 2007 में विकसित किया, स्ट्रिंग कंकटिनेशन के लिए "+" ऑपरेटर का प्रयोग करता है, जो कि जावा और सी ++ की तरह है। 

### विकल्प:

आप `strconv.Itoa` फ़ंक्शन का भी उपयोग कर सकते हैं अगर आप int और स्ट्रिंग के बीच में कनवर्ट करना चाहते हैं। 

### विफलता विवरण:

स्ट्रिंग्स Go में यादृच्छिक होती हैं, इसे सक्षम बनाते हैं कि कम संसाधनों के साथ कुशलता और सुरक्षा को बनाए रखें।

## भी देखें:

अधिक जानकारी के लिए, आप [A Tour of Go](https://tour.golang.org/welcome/1) और [Effective Go](https://golang.org/doc/effective_go.html) देख सकते हैं।