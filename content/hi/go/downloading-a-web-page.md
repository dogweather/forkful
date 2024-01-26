---
title:                "वेब पेज डाउनलोड करना"
date:                  2024-01-20T17:44:15.698233-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वेब पेज डाउनलोड करना मतलब है इंटरनेट से HTML डेटा को आपके कंप्यूटर पर सेव करना। प्रोग्रामर्स इसे डेटा विश्लेषण, वेब स्क्रैपिंग, और ऑफलाइन ब्राउजिंग के लिए करते हैं।

## How to: (कैसे करें:)
```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	url := "http://example.com"
	resp, err := http.Get(url)
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	defer resp.Body.Close()

	html, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Println("Error reading HTML:", err)
		return
	}

	fmt.Println(string(html))
}
```
इस कोड के चलने पर, आपको `example.com` का HTML डेटा दिखेगा।

## Deep Dive (गहराई में जानकारी)
वेब पेज डाउनलोडिंग की शुरुआत इंटरनेट के साथ ही हो गई थी। `http.Get` फंक्शन गो का एक पार्ट है जिसे `net/http` पैकेज में define किया गया है, और ये HTTP प्रोटोकॉल का इस्तेमाल करके वेब से डेटा उठाता है। विकल्पों में आपको curl लाइब्रेरीज, Selenium जैसे ब्राउज़र ऑटोमेशन टूल्स, और अन्य भाषाओं के HTTP लाइब्रेरीज मिलेंगे। बेस्ट प्रैक्टिस में फंक्शन्स का सही तरीके से हैंडल करना, एरर चेकिंग, और `defer` का इस्तेमाल करना शामिल है, जैसा कि ऊपर के उदाहरण में दिखाया गया है।

## See Also (और जानकारी के लिए)
- Go के ऑफिसियल डॉक्युमेंटेशन में `net/http` पैकेज: https://golang.org/pkg/net/http/
- वेब स्क्रैपिंग के लिए Go ट्यूटोरियल: https://scrapinghub.com
- HTTP प्रोटोकॉल के बारे में MDN डॉक्युमेंटेशन: https://developer.mozilla.org/en-US/docs/Web/HTTP
