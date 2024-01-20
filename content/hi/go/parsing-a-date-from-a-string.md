---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों (What & Why?) 

एक तारिक या समय को स्ट्रिंग से पार्स करना इसका ढांचा समझने और निर्दिष्ट तारिक-समय घटकों को प्राप्त करने का काम है। इसे प्रबंधन, मान्यता और तुलना के लिए प्रोग्रामर करते हैं।

## कैसे करें (How to)

गो में, आप `time` पैकेज का 'Parse' फंक्शन उपयोग करके इसे पार्स कर सकते हैं:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t, _ := time.Parse("2006-01-02", "2021-09-15")
	fmt.Println(t)
}
```
निष्पादन करने पर, आपको निम्नलिखित परिणाम मिलेगा:
```Go
2021-09-15 00:00:00 +0000 UTC
```

## गहरी गोता (Deep Dive)

1) ऐतिहासिक संदर्भ: गो प्रोग्रामिंग भाषा में `time` पैकेज 2007 में जोड़ी गई थी। इसे दिनांक, समय, और टाइमज़ोन से काम करने के लिए बनाया गया है।
2) विकल्प: `ParseStrict` जो मात्र कठोरकोड किए गए स्ट्रिंग से पार्स कर सकता है। अन्य भाषाओं जैसे कि Python में डेट-टाइम पार्स के लिए कस्टम लाइब्रेरी उपलब्ध हैं।
3) क्रियान्वयन विवरण: 'Parse' फ़ंक्शन आधारभूत पैकेज प्रबंधन का उपयोग करती है। इस प्रक्रिया का अर्थ है कि यदि स्ट्रिंग सही फॉर्मेट में नहीं है, तो यह असफल हो सकता है।

## और भी देखें (See Also)

1) [Go Docs: Time Package](https://golang.org/pkg/time/) : गो दस्तावेजीकरण में `time` पैकेज के बारे में.
2) [Understanding Time in Go](https://medium.com/rungo/understanding-the-t...): टाइम पैकेज को गहराई से समझने के लिए इस ब्लॉग पोस्ट को पढ़ें।
3) [Date Parsing in Other Languages](https://stackoverflow.com/questions/tagged/date-parsing): अन्य भाषाओं में तारीख पार्स करने के तरीकों कि जानकारी के लिए।