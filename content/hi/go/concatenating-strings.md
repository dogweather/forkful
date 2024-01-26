---
title:                "स्ट्रिंग को जोड़ना"
date:                  2024-01-20T17:34:46.247002-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
जब हम दो या दो से अधिक strings को जोड़ते हैं, उसे concatenating कहते हैं। Programmers अक्सर डाटा प्रोसेस करते समय या messages बनाते समय इसका इस्तेमाल करते हैं।

## How to: (कैसे करें:)
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// सरलतमconcatenation using '+'
	hello := "नमस्ते"
	world := "दुनिया"
	helloWorld := hello + " " + world
	fmt.Println(helloWorld) // "नमस्ते दुनिया"

	// strings.Join का उपयोग
	words := []string{"नमस्ते", "दुनिया", "from", "Go"}
	sentence := strings.Join(words, " ")
	fmt.Println(sentence) // "नमस्ते दुनिया from Go"
	
	// strings.Builder का प्रयोग
	var builder strings.Builder
	for _, word := range words {
		builder.WriteString(word)
		builder.WriteRune(' ')
	}
	fmt.Println(builder.String()) // "नमस्ते दुनिया from Go "
}
```

## Deep Dive (गहराई से जानकारी)
शुरुआत में, Go में स्ट्रिंग्स immutable थी, इसलिए concatenation एक नया स्ट्रिंग बनाती थी। बड़ी फाइलों और loops के साथ यह inefficient हो जाता था। `strings.Builder` और `strings.Join` जैसे tools को बाद में जोड़ा गया। ये memory का बेहतर प्रयोग करते हैं और परफॉर्मेंस बढ़ाते हैं।

## See Also (इसके अलावा)
- String processing in Go Docs: [Go Docs](https://golang.org/pkg/strings/)
- Blog post about Go slice tricks: [Go Blog](https://blog.golang.org/slices)
- Effective Go Document: [Effective Go](https://golang.org/doc/effective_go.html#slices)
