---
title:                "नियमित अभिघातों का उपयोग करना"
html_title:           "Go: नियमित अभिघातों का उपयोग करना"
simple_title:         "नियमित अभिघातों का उपयोग करना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप अपने डेटा को खोजने और छानने के लिए लंबे संकेतों का उपयोग करते हैं? या फिर आप चाहते हैं कि आपके कोड में दिए गए चरित्रों की सीमाओं को समायोजित किया जा सके? अगर हाँ, तो आपको Regular Expressions के बारे में जानना चाहिए। यह Go भाषा में एक शक्तिशाली और सुविधाजनक फंक्शन है जो आपको अपने टेक्स्ट को प्रोसेस करने में मदद कर सकता है।

## कैसे करें

गो में रेगुलर एक्सप्रेशन का उपयोग करना बहुत ही आसान है। आप बस `regexp` पैकेज को अपने कोड में इम्पोर्ट करने से शुरू कर सकते हैं। फिर आप चाहे तो `regexp.Compile()` या `regexp.MustCompile()` का उपयोग करके अपने इनपुट को प्रोसेस कर सकते हैं। इसके बाद आप `FindString()` या `FindStringSubmatch()` फंक्शन का उपयोग करके मैचिंग चरित्रों की सूची प्राप्त कर सकते हैं। नीचे दिए गए कोड ब्लॉक में आप दो उदाहरण देख सकते हैं।

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// रेगुलर एक्सप्रेशन का उपयोग करके फिर्स्ट नेम निकालना
	text := "मेरा नाम है जॉन"
	re := regexp.MustCompile(`मेरा नाम है (.+)`)
	match := re.FindStringSubmatch(text)
	fmt.Println(match[1]) // जॉन

	// रेगुलर एक्सप्रेशन का उपयोग करके ईमेल वैधता बनाना
	email := "john@doe.com"
	re = regexp.MustCompile(`[a-zA-Z0-9]+@[a-zA-Z0-9]+\.[a-zA-Z0-9]+`)
	valid := re.MatchString(email)
	fmt.Println(valid) // true
}
```

## गहराई में जाएं

रेगुलर एक्सप्रेशन को समझना और मास्टर करना थोड़ा समय लग सकता है, लेकिन वह आपको बहुत लाभ प्रद