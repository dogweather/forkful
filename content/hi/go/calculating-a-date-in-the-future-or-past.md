---
title:                "भविष्य या अतीत में तारीख की गणना"
date:                  2024-01-20T17:31:44.650750-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
तारीख की गणना मतलब भूतकाल या भविष्यकाल में तारीख जोड़ना या घटाना। प्रोग्रामर इसका इस्तेमाल डेडलाइन्स, इवेंट्स या अन्य समय-संबंधित कार्यक्रमों की प्लानिंग के लिए करते हैं।

## कैसे करें? (How to:)
```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// आज की तारीख
	today := time.Now()
	fmt.Println("आज की तारीख:", today.Format("02-Jan-2006"))

	// 10 दिन बाद की तारीख
	future := today.AddDate(0, 0, 10)
	fmt.Println("10 दिन बाद की तारीख:", future.Format("02-Jan-2006"))

	// 10 दिन पहले की तारीख
	past := today.AddDate(0, 0, -10)
	fmt.Println("10 दिन पहले की तारीख:", past.Format("02-Jan-2006"))
}
```

मान लीजिए आज की तारीख है 1 अप्रैल 2023, तो आउटपुट होगा:
```
आज की तारीख: 01-Apr-2023
10 दिन बाद की तारीख: 11-Apr-2023
10 दिन पहले की तारीख: 22-Mar-2023
```

## गहन अध्ययन (Deep Dive)
पहले के समय में, तारीखों की गणना मैनुअल रूप से की जाती थी, जिसमें समय और गलतियों का अधिक मौका होता था। गो (Go) जैसी प्रोग्रामिंग भाषाएं, जो कि वर्ष 2009 में गूगल द्वारा बनाई गई थी, समय संबंधित कार्यो को आसान और सटीक बनाते हैं। `time` पैकेज का 'AddDate' फंक्शन वर्ष, महीने, और दिन को जोड़ने या घटाने में मदद करता है। इसके अलावा, `time.Duration` और `time.Add` का प्रयोग और भी छोटे समय-अंतराल के लिए होता है।

## संबंधित लिंक्स (See Also)
- Go के `time` पैकेज के प्रलेखन (Documentation for Go's `time` package): https://pkg.go.dev/time
- Go by Example: Time - गो के उदाहरण द्वारा समय संबंधी ट्यूटोरियल: https://gobyexample.com/time
- Go के समय फॉर्मेट नियम (Rules for time format in Go): https://golang.org/src/time/format.go
