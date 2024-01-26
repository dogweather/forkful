---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:31:47.668566-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTML को पार्स करना मतलब वेब पेज की मार्कअप को समझना और उसे संसाधित करना है। प्रोग्रामर्स इसे डेटा निकालने, वेबसाइट्स की जाँच-पड़ताल करने या ऑटोमेशन के लिए करते हैं।

## How to: (कैसे करें:)
```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"net/http"
	"strings"
)

func main() {
	resp, err := http.Get("https://example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	doc, err := html.Parse(resp.Body)
	if err != nil {
		panic(err)
	}

	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "a" {
			for _, a := range n.Attr {
				if a.Key == "href" {
					fmt.Println(a.Val)
					break
				}
			}
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}
	f(doc)
}
```
सैंपल आउटपुट:
```
https://www.iana.org/domains/example
```

## Deep Dive (गहराई से जानिए)
HTML पार्सिंग का इतिहास वेब के शुरुआती दिनों से चला आ रहा है। `golang.org/x/net/html` पैकेज Go मे बिना किसी बाहरी पुस्तकालयों की जरूरत के HTML पार्स करने की अनुमति देता है। हालांकि, अन्य विकल्प जैसे `GoQuery` या स्क्रेपिंग टूल्स भी हैं जो jQuery की शैली में सुविधाएँ प्रदान करते हैं। पार्सिंग जटिल हो सकती है क्योंकि HTML में गलतियाँ और अनियमितताएँ हो सकती हैं, लेकिन Go के पैकेज में इन चुनौतियों से निपटने के लिए रोबस्ट एल्गोरिद्म हैं।

## See Also (इसे भी देखें)
- GoQuery के बारे में और जानिए: https://github.com/PuerkitoBio/goquery
- Go के html पैकेज का डॉक्स: https://pkg.go.dev/golang.org/x/net/html
- HTML पार्सिंग शुरुआती गाइड: https://www.owasp.org/index.php/Web_2.0_Security_Cheat_Sheet#HTML_Parsing
