---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Go Programming ke Duniya me: Dates Ki Tulna

## क्या और क्यूँ?

दो तारीखों की तुलना (comparing two dates) यह होती है जब हम पता लगाने की कोशिश करते हैं कि एक दिनांक (date) दूसरे दिनांक से पहले, बाद में, या उनके बीच में है। यह समय-मूल्यांकन लॉजिक में एक आवश्यक कार्य होता है, जैसे कि कृपया आपको समझाने के लिए पता करें कि कौन सा event सबसे पहले हुआ था, या किसी कार्य की सीमा कब समाप्त होगी।

## कैसे करे:

```Go
package main
import (
    "fmt"
    "time"
)

func main() {
    date1 := time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC)
    date2 := time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC)
    if date1.Before(date2) {
        fmt.Println("Date1 is before Date2")
    } else if date1.After(date2) {
        fmt.Println("Date1 is after Date2")
    } else {
        fmt.Println("Both dates are equal")
    }
}
```

## गहराई में:

दो तारीखों की तुलना न कर पाने वाली परम्परागत कम्पाइल भाषाओं केवलं है। स्थिरता और अनुकूलन को बाना में रखते हुए, Go ने इस क्षमता को बहुत ही सीधा और स्वच्छ तरीका बनाया है। अल्टरनेटिवली, हम `Equal()` और `Sub()` जैसे फ़ंक्शन्स को भी उपयोग कर सकते हैं, जो अरबी इकाइयों के लिए सही और आवश्यक हो सकते हैं।

## भी देखें:

1. Go के तारीख और समय संबंधी फ़ंक्शन्स के बारे में अधिक जानकारी के लिए यहाँ जाएं: https://golang.org/pkg/time/
2. Go में समय की तुलना का तरीका: https://yourbasic.org/golang/compare-dates/
3. डेट की तुलना के बारे में विस्तृत डॉक्यूमेंटेशन के लिए StackOverflow ब्लॉग पर ध्यान दें: https://stackoverflow.com/questions/36530251/how-to-compare-two-dates