---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:33:48.242447-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"

category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीखों की तुलना करना समय के दो पलों के बीच अंतर को समझने की प्रक्रिया है। प्रोग्रामर्स यह तुलना डेटा की वैधता, समय-सीमा निर्धारित करना, और समय संबंधित गणना के लिए करते हैं।

## How to: (कैसे करें:)
Go में दो तारीखों का तुलना करना आसान है। `time` पैकेज का प्रयोग करें। नीचे एक उदाहरण है:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // तारीखों का परिभाषित करना
    layout := "2006-01-02"
    date1, _ := time.Parse(layout, "2023-04-01")
    date2, _ := time.Parse(layout, "2023-04-12")

    // तारीखों की तुलना करना
    if date1.Before(date2) {
        fmt.Println("date1 पहले है date2 से")
    } else if date1.After(date2) {
        fmt.Println("date1 बाद में है date2 से")
    } else {
        fmt.Println("date1 और date2 समान हैं")
    }

    // तारीखों के बीच अंतर का पता लगाना
    diff := date2.Sub(date1)
    fmt.Printf("दोनों तारीखों के बीच का अंतर: %v\n", diff)
}
```

संभावित आउटपुट:

```
date1 पहले है date2 से
दोनों तारीखों के बीच का अंतर: 264h0m0s
```

## Deep Dive (गहराई से जानकारी):
Go में `time` पैकेज `time.Time` प्रकार से तारीखों को संभालता है, जिसके बहुत से उपयोगी फंक्शन्स हैं। `Before`, `After`, और `Equal` फंक्शन्स से हम तारीखों की तुलना करते हैं। `Sub` फंक्शन दो तारीखों के बीच के समय का अंतर देता है। इतिहास में, प्रोग्रामर्स को यह सारे गणनाएँ खुद से करने पड़ते थे, पर Go ने इसे सरल बना दिया है। अल्टरनेटिव्स में `date` पैकेज और दूसरे लाइब्रेरीज शामिल हैं, लेकिन Go का स्टैण्डर्ड लाइब्रेरी बहुत सक्षम है।

## See Also (और भी देखें):
- Go दस्तावेज़ीकरण `time` पैकेज: [https://golang.org/pkg/time/](https://golang.org/pkg/time/)
- Go टाइम पार्सिंग और फॉर्मेटिंग: [https://yourbasic.org/golang/format-parse-string-time-date-example/](https://yourbasic.org/golang/format-parse-string-time-date-example/)
- Go के `date` पैकेज भी देखें: [https://github.com/rickb777/date](https://github.com/rickb777/date)
