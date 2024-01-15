---
title:                "तारीख प्राप्त करना"
html_title:           "Go: तारीख प्राप्त करना"
simple_title:         "तारीख प्राप्त करना"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप भी सोचते हैं कि किसी भी प्रोग्रामिंग भाषा ने कुछ न कुछ तारीख और समय से सम्बंधित काम करने के लिए विशेष तरीके तैयार किए होंगे? गो (Go) भी इसमें अग्रणी है। यहां आप जानेंगे कि गो (Go) में आज की तारीख कैसे प्राप्त की जाती है और उसका उपयोग कैसे किया जाता है।

## कैसे

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    current_date := time.Now() // वर्तमान तारीख प्राप्त करना
    fmt.Println("आज की तारीख:", current_date)
}
```

इस कोड ब्लॉक में हमने सबसे पहले गो (Go) के `time` पैकेज का उपयोग करके वर्तमान तारीख को प्राप्त किया है। फिर हमने उसे `fmt` पैकेज के `Println()` फंक्शन का उपयोग करके प्रिंट किया है। आप देख सकते हैं कि उपरोक्त कोड का आउटपुट निम्नवत है:

```
आज की तारीख: 2021-10-17 18:34:34.4915352 +0530 IST m=+0.003991401
```

आप इसी तरह दूसरे प्रकार के तारीख और समय के साथ भी खेल सकते हैं। जैसे कि आप वर्तमान दिन या महीने को भी प्राप्त कर सकते हैं। नीचे दिए गए कोड से आपको इसका उपयोग करने का साफ़ अनुमान लग सकता है:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    current_date := time.Now()
    month := current_date.Month() //वर्तमान महीना प्राप्त करना
    fmt.Println("वर्तमान महीना:", month)

    day := current_date.Day() //वर्तमान दिन प्राप्त करना
    fmt.Println("वर्तमान दिन:", day)

    year := current_date.Year() // वर्तमान साल प्राप्त करना
    fmt.Println("वर्तमान साल:", year)
}
```

उपरोक्त कोड का आउटपुट इस प्रक