---
title:                "दो तारीखों की तुलना करना"
html_title:           "Go: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

What & Why?
दो तारीखों को तुलना करना तभी होता है, जब हमें दो अलग समय अवधियों में घटनाओं की तुलना करनी हो। प्रोग्रामर्स इस काम को करते हैं ताकि वे खुद से कोड करने के समय और असामान्य तारीखों को समझने में मदद मिल सके।

How to:
Go में दो तारीखों को तुलना करने के कई तरीके हैं। एक सेंसिटिविटी-आज मूल्यवान मापदंड के साथ तारीखों को तुलना कर सकते हैं। नीचे एक उदाहरण है:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	const longForm = "January 2, 2006"
	t1, _ := time.Parse(longForm, "March 15, 2020")
	t2, _ := time.Parse(longForm, "March 14, 2020")

	if t1.After(t2) {
		fmt.Println(t1, "is greater than", t2)
	} else if t1.Before(t2) {
		fmt.Println(t1, "is less than", t2)
	} else {
		fmt.Println("The dates are equal.")
	}
}
```

यह कोड निम्नलिखित आउटपुट देगा:

```Go
2020-03-15 00:00:00 +0000 UTC is greater than 2020-03-14 00:00:00 +0000 UTC
```

Deep Dive:
इतिहास में, लोग दो तारीखों को तुलना करने के लिए अपने अंग्रेजी कैलेंडर की मदद से तारीखों को अनुक्रमित करते थे। लेकिन आजकल प्रोग्रामर्स इसे तारीख और समय कोडिंग करने के दौरान समझने के लिए निश्चित तारीख पर अपने समय को कैलेंडर के साथ दिखाते हैं। यह कोड खनन और अर्थपुर्णता को आसान बनाने के लिए मदद करता है। अन्य विकल्पों में एम्बेडेड रिक्वेवल्वर और দ딎됝긨। तारीखों को तुलना करने की विस्तृत जानकारी के लिए [Go Documentation](https://golang.org/pkg/time/#pkg-overview) देखें।

See Also:
- [Go Documentation](https://golang.org/pkg/time/#pkg-overview)
- [Blog post on date comparisons in Go](https://blog.golang.org/organizing-go-code)