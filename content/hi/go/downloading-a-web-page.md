---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वेब पेज डाउनलोड करना मतलब इंटरनेट से सूचना एकत्र करना। यह डेवलपर्स टास्क ऑटोमेटेट करने, डाटा एनालायज़, और वेब स्क्रेपिंग के लिए होता हैं।

## कैसे करें:

यहां Go लैंग्वेज में एक कोड उदाहरण है कि कैसे वेब पेज डाउनलोड किया जाता है:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	resp, err := http.Get("http://example.com/")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}

	fmt.Println(string(body))
}
```

यह साधारण आउटपुट देता है:

```
<!doctype html>
<html>
<head>
<title>Example Domain</title>
...
</html>
```

## गहराई से जानकारी:

- **ऐतिहासिक सन्दर्भ:** वेब पेज डाउनलोडिंग का उपयोग १९९० के दशक से देवलपर्स कर रहे हैं।
- **विकल्प:** साइट हिट करने के अन्य लैंग्वेज में कोड लिखने के विकल्प भी हैं, जैसे कि Python, Java और JavaScript।
- **कैसे काम करता है:** Go कोड सीधे टीसीपी/आईपी प्रोटोकॉल पर रिक्वेस्ट भेजता है। ये रिक्वेस्ट वेबसर्वर तक पहुंचती हैं, जो उससे जुड़ी HTML सूचना को डेटा पैकेट में लौटाता है।

## इसके साथ देखिए:

- Go में [वेब स्क्रेपिंग](https://yourbasic.org/golang/web-scraping/) कैसे करें।
- HTTP रिक्वेस्ट के बारे में और जानने के लिए [Go Docs](https://golang.org/pkg/net/http/) का उपयोग करें।