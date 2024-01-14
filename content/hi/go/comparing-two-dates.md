---
title:                "Go: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप अपने प्रोग्राम में दो तारीखों को तुलना करने की जरूरत है? आमतौर पर, तारीख की तुलना एप्लिकेशन में दो तारीखों के बीच किए गए बदलावों को प्रबंधित करने के लिए की जाती है। इस लेख में, हम दो तारीखों की तुलना के लिए गो प्रोग्रामिंग का उपयोग करने के बारे में आपको बताएँगे।

## कैसे करें

अगर आप गो प्रोग्रामिंग में दो तारीखों की तुलना करना चाहते हैं, तो निम्नलिखित कोड उदाहरण आपको मदद करेंगे। चलिए इसे एकाग्रता से देखते हैं।

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// पहली तारीख
	firstDate := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)

	// दूसरी तारीख
	secondDate := time.Date(2021, time.February, 1, 0, 0, 0, 0, time.UTC)

	// बेजोड़ तारीखों को तुलना करें
	if firstDate.Equal(secondDate) {
		fmt.Println("दोनों तारीखों बराबर हैं!")
	} else {
		fmt.Println("दोनों तारीखों में अंतर है।")
	}

	// तारीख का अंतर प्राप्त करें
	diff := secondDate.Sub(firstDate)
	fmt.Println(diff.Hours(), "घंटे का फ़र्क़ है।")
}
```

आपको यहां स्पष्ट दिखाई देना चाहिए।

## डीप डाइव

अब आप गो प्रोग्रामिंग के माध्यम से तारीखों की तुलना को और गहराई से समझ सकते हैं। गो में तारीखों को प्रबंधित करने के लिए बहुत सारे फ़ंक्शन हैं जैसे time.Now (), time.Parse (), time.Unix () आदि। आपको यह समझना महत्वपूर्ण है कि गो में तारीख हमेशा एक time.Time ऑब्जेक्ट के रूप में ही संरच