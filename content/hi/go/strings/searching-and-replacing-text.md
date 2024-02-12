---
title:                "पाठ को खोजना और बदलना"
date:                  2024-02-03T18:09:13.783375-07:00
model:                 gpt-4-0125-preview
simple_title:         "पाठ को खोजना और बदलना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/searching-and-replacing-text.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

प्रोग्रामिंग में टेक्स्ट की खोज और प्रतिस्थापन डेटा में संशोधन और प्रबंधन की सहायता करता है, जो कि सॉफ्टवेयर विकास और डेटा हेरफेर में एक मूलभूत कार्य है। प्रोग्रामर्स ये ऑपरेशन टेक्स्टुअल डेटा को कुशलता से अपडेट, साफ या परिवर्तित करने के लिए करते हैं।

## कैसे:

Go में, `strings` पैकेज विभिन्न फ़ंक्शंस प्रदान करता है ताकि स्ट्रिंग्स के भीतर टेक्स्ट को खोजने और प्रतिस्थापित करने में सहायता मिल सके। चलिए कुछ आम तरीकों का पता लगाते हैं।

**`strings.Contains` का उपयोग करके टेक्स्ट की खोज करना:**

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	myString := "Hello, Go programmers!"
	fmt.Println(strings.Contains(myString, "Go"))  // आउटपुट: true
	fmt.Println(strings.Contains(myString, "Java")) // आउटपुट: false
}
```

**`strings.Replace` और `strings.ReplaceAll` के साथ टेक्स्ट को प्रतिस्थापित करना:**

`strings.Replace` आपको एक स्ट्रिंग के भीतर उपस्ट्रिंग को प्रतिस्थापित करने की अनुमति देता है, जितने प्रतिस्थापन करने हैं उसकी संख्या निर्दिष्ट करते हुए, जबकि `strings.ReplaceAll` सभी उदाहरणों को प्रतिस्थापित करता है।

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	myString := "Hello, Go! Go is fun."
	fmt.Println(strings.Replace(myString, "Go", "Golang", 1))  // आउटपुट: Hello, Golang! Go is fun.
	fmt.Println(strings.ReplaceAll(myString, "Go", "Golang")) // आउटपुट: Hello, Golang! Golang is fun.
}
```

**उन्नत खोज और प्रतिस्थापन के लिए `regexp` पैकेज का उपयोग करना:**

अधिक जटिल पैटर्न के लिए, `regexp` पैकेज बेहद शक्तिशाली होता है, नियमित अभिव्यक्तियों (regular expressions) का समर्थन करता है।

```go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	myString := "Hello, Go programmers! Go is fun."
	re := regexp.MustCompile(`Go`)
	fmt.Println(re.ReplaceAllString(myString, "Golang"))  // आउटपुट: Hello, Golang programmers! Golang is fun.
}
```

## गहराई से जानकारी

Go में, टेक्स्ट में मैनिपुलेशन, खोज और प्रतिस्थापन ऑपरेशन समेत, सरल और कुशल डिज़ाइन किया गया है, Go की व्यापक मानक लाइब्रेरी का लाभ उठाते हुए। `strings` पैकेज आम उपयोग के मामलों के लिए बेसिक कार्यक्षमताओं को प्रदान करता है, जबकि `regexp` पैकेज अधिक जटिल पैटर्नों की मांग करने वाले मामलों के लिए उपयुक्त है जिनमें नियमित अभिव्यक्तियों की आवश्यकता होती है।

ऐतिहासिक तौर पर, Go का दृष्टिकोण स्ट्रिंग्स और टेक्स्ट मैनिपुलेशन को हैंडल करने में सादगी और प्रदर्शन पर जोर देने का रहा है। `strings` और `regexp` जैसे शक्तिशाली पैकेजों को मानक लाइब्रेरी का हिस्सा बनाने का निर्णय इस इच्छा से प्रेरित था कि Go को वेब विकास और टेक्स्ट प्रोसेसिंग एप्लिकेशनों के लिए एक व्यावहारिक विकल्प बनाया जा सके, जहां ऐसे ऑपरेशन अक्सर होते हैं।

यह ध्यान देने योग्य है कि जबकि Go के `strings` और `regexp` पैकेज जरूरतों की व्यापक श्रृंखला को कवर करते हैं, कुछ स्थितियों में अन्य भाषाएँ या विशेषीकृत लाइब्रेरीज यूनिकोड हैंडलिंग या प्राकृतिक भाषा प्रोसेसिंग के क्षेत्र में अधिक उन्नत टेक्स्ट मैनिपुलेशन सुविधाएँ प्रदान कर सकती हैं। हालाँकि, सॉफ्टवेयर विकास में खोज और प्रतिस्थापन कार्यों के बहुसंख्यक के लिए, Go पहले से ही बॉक्स के बाहर मजबूत और कुशल उपकरण प्रदान करता है।
