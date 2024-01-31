---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:14:42.355610-07:00
simple_title:         "वर्तमान तारीख प्राप्त करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वर्तमान तिथि प्राप्त करने का मतलब है सिस्टम का समय पुकारना। प्रोग्रामर्स ऐसा लॉगिंग, टाइमस्टैम्प्स और समय पर आधारित फंक्शनालिटी के लिए करते हैं।

## How to: (कैसे करें:)
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now() // वर्तमान तिथि और समय
	fmt.Println("वर्तमान समय है:", currentTime)
}
```

सैंपल आउटपुट:
```
वर्तमान समय है: 2023-03-29 12:04:05.123456789 +0530 IST m=+0.000123456
```

## Deep Dive (गहराई से जानकारी)
Go में, `time` पैकेज समय से संबंधित सभी कार्यों को संभालता है। यह पैकेज Go के साथ विकसित होता गया है, इसलिए यह सटीक और लचीला है। `time.Now()` फंक्शन सिस्टम क्लॉक से वर्तमान समय पाने का सीधा तरीका है। विकल्पों में `time.Ticker` और `time.Timer` जैसे टूल्स भी शामिल हैं जो निर्धारित समय के बाद कार्य चलाने में मददगार हैं। Go की समय संबंधित कार्यक्षमता बहुत ही व्यापक है और इसमें समय ज़ोन, टाइमस्टैम्प्स, और ड्यूरेशन जैसे महत्वपूर्ण अवधारणाओं को संभालने का समावेश है।

## See Also (और भी जानकारी)
- Go `time` package documentation: [pkg.go.dev/time](https://pkg.go.dev/time)
- Go by Example: Time: [gobyexample.com/time](https://gobyexample.com/time)
- Go Time Formatting and Parsing: [yourbasic.org/golang/format-parse-string-time-date-example](https://yourbasic.org/golang/format-parse-string-time-date-example)
