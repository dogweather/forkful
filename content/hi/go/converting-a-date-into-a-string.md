---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
दिनांक को स्ट्रिंग में बदलना इसका अर्थ होता है कि हम एक दिनांक को टेक्स्ट फॉर्मेट में बदल देते हैं। प्रोग्रामर्स इसे तभी करते हैं जब हमें उपयोगकर्ता के लिए संगत तिथि-समय प्रारूप की आवश्यकता होती है।

## कैसे करें:
`Go` में, हम `time` पैकेज का उपयोग करते हैं दिनांक को स्ट्रिंग में बदलने के लिए।

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// वर्तमान समय प्राप्त करें
	t := time.Now()
	
	// समय को स्ट्रिंग में कन्वर्ट करें
	s := t.Format("2006-01-02")

	fmt.Println(s) // "YYYY-MM-DD" प्रिंट करें
}
```

इस कोड का आउटपुट आपकी स्थानीय समय क्षेत्र के अनुसार वर्तमान दिनांक होगा।


## गहराई में:
स्थिति और आवश्यकताएं अनुसार, आप अलग-अलग दिनांक / समय प्रारूप उपयोग कर सकते हैं। जैसे "Jan 2, 2006 at 3:04pm (MST)"या "Monday, 02-Jan-06 15:04:05 PST"।"`Format` मेठड `time` पैकेज में परिभाषित है और यह एक स्ट्रिंग परामीटर लेता है जो आपके दिनांक / समय प्रारूप निर्दिष्ट करता है।

`Go` मे दिनांक / समय प्रारूप का एक विशेष तरीका है जिसे "आंकड़े" समय कहा जाता है और इसे "2006-01-02 15:04:05" के रूप में दर्शाया गया है।

## देखें भी: 

1. Go दस्तावेज़ीकरण: [time package](https://pkg.go.dev/time?tab=doc)
2. "A Tour of Go: [Formatting Times and Dates](https://tour.golang.org/welcome/4)"
3. Stackoverflow: ["How to format a time.Time"](https://stackoverflow.com/questions/20234104/how-to-format-current-time-using-a-yyyymmddhhmmss-format)