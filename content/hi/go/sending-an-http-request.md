---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP (एचटीटीपी) अनुरोध भेजना एक तरह का दूसरे सर्वर तक संचार है। प्रोग्रामर्स इसे डाटा प्राप्त करने, बदलने या वेबसाइटों के साथ इंटरैक्ट करने के लिए करते हैं।

## कैसे करें
यहां आपको Go का उपयोग करके HTTP अनुरोध कैसे भेजना चाहिए, उसका एक उदाहरण दिया गया है।
```Go
package main

import (
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	resp, err := http.Get("http://webcode.me")
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}

	log.Println(string(body))
}
```
इस कोड ने http.Get का उपयोग करके "http://webcode.me" URL से HTTP अनुरोध भेजा। यदि कोई त्रुटि होती है, तो यह त्रुटि लॉग करेगा। अन्यथा, यह लॉग करेगा HTTP रिस्पॉन्स बॉडी।

## गहरा डाइव
जब इंटरनेट का निर्माण हुआ था, तब HTTP की आवश्यकता हुई थी क्योंकि यह वेबसाइटों और ब्राउज़रों के बीच जानकारी साझा करने का एक मानक तरीका है। Historic context के हिसाब से HTTP/1.1 से पहले HTTP/0.9 (1991) और HTTP/1.0 (1996) थे। HTTP/2 (2015) और HTTP/3 (2020) आधिकारिक रूप से HTTP/1.1 (1997) के बाद पेश किए गए। 

वैकल्पिक रूप से, प्रोग्रामर्स अन्य प्रोग्रामिंग भाषाओं, जैसे कि Python's `requests` लाइब्रेरी या JavaScript's `fetch` API, का उपयोग कर सकते हैं। 

Go में, `http.Get` फंक्शन सबसे सरल HTTP अनुरोध बनाने का तरीका है। यह `http.Response` और `error` लौटाता है। आप उत्तर में दिए गए डेटा को आसानी से पढ़ सकते हैं `ioutil.ReadAll` का उपयोग करके।

## देखें भी
- [Go दस्तावेज़ीकरण](https://golang.org/pkg/net/http/)
- [HTTP - Wikipedia](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
- [MDN Web Docs - HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [http.Get फ़ंक्शन का स्रोत कोड](https://golang.org/src/net/http/client.go)