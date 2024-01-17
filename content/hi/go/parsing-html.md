---
title:                "होमपेज को विश्लेषण करने का तरीका"
html_title:           "Go: होमपेज को विश्लेषण करने का तरीका"
simple_title:         "होमपेज को विश्लेषण करने का तरीका"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/parsing-html.md"
---

{{< edit_this_page >}}

## परिचय

HTML को पार्स (parse) करना एक आम क्रिया है जो वेब डेवलपर्स को रेंतेवेबल और लागू संचित जानकारी देने के लिए की जाती है। यह कोड, टेक्स्ट और मल्टीमीडिया कंटेंट को सांझा करने के लिए स्ट्रक्चर्स के रूप में काम करता है। वेब पृष्ठों के स्ट्रक्चर को समझने के लिए, वेब डेवलपर्स उपलब्ध HTML को पार्स करते हैं।

## कैसे:

जाओ कोड के जरिए प्रैक्टिकल उदाहरण सिखाएं:
```Go
package main

import (
	"fmt"
	"strings"

	"golang.org/x/net/html"
)

func main() {
	htmlString := "<html><head><title>This is a title</title></head><body><h1>Heading</h1><p>This is a paragraph</p></body></html>"

	r := strings.NewReader(htmlString)
	doc, err := html.Parse(r)
	if err != nil {
		fmt.Println("Error parsing HTML!")
	}

	var traverse func(*html.Node)
	traverse = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "h1" {
			fmt.Println("Heading found!")
		}
		if n.Type == html.ElementNode && n.Data == "p" {
			fmt.Println("Paragraph found!")
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			traverse(c)
		}
	}
	traverse(doc)
}
```

आउटपुट:
```
Heading found!
Paragraph found!
```

## गहराई में जानकारी:

HTML को पार्स करने का इतिहास 1991 में आईएनएसजी के इयान हैंटेंन के द्वारा बनाया गया था। इससे पहले, एक जेनेरेटेड सुविधा का उपयोग किया जाता था जिससे HTML को ट्रैन्सलेट करें और पेश करें। लेकिन प्रोग्रामर्स इस सुविधा को अनकवरेजी और नायांकों के प्रवाह के समस्याओं के कारण छोड़ रहे थे। अब यहां, HTML को पार्स करने के लिए बहुत सारे विकल्प हैं जैसे कि एचएमएल, सि, पायथन आदि। गो पोपुलर, विकसित, तेजी से संपादन होने और मजबूत होने के कारण, वेब डेवलपरों को HTML को पार्स करने के लिए अधिक लोकप्रिय विकल्प में से एक है। गो के आरामदेह कोडिंग संरक्षण को ध्यान में रखते हुए, अनुभवी प्रोग्रामर्स को संबंधित कोडिंग कुशलताओं को पालन करने के लिए इसमें सलाह दी जाती है।

## अन्य संसाधनों को देखें:

- https://golang.org/pkg/net/html/
- https://www.w3schools.com/html/html_intro.asp
- https://developer.mozilla.org/en-US/docs/Web/HTML