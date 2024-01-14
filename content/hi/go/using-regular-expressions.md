---
title:                "Go: नियमित अभिव्यक्तियों का उपयोग करना"
simple_title:         "नियमित अभिव्यक्तियों का उपयोग करना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों
रेग्युलर एक्सप्रेशन्स का इस्तेमाल क्यों किया जाता है, इसके पीछे की वजह समझना बहुत महत्वपूर्ण है। ये फाइल सर्चिंग, पाठों संशोधन और टेक्स्ट प्रोसेसिंग में बहुत उपयोगी हैं।

## कैसे करें
"```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	input := "Hello, Go programming is fun!"
	re := regexp.MustCompile(`[a-z]+`) // इस एक्सप्रेशन में हमने सारे छोटे अक्षर खोजे हैं

	matches := re.FindAllString(input, -1)
	fmt.Printf("%#v", matches) // यहाँ हमारे खोजे हुए अक्षर हैं
}
"```

आउटपुट:
[]string{"ello", "o", "programming", "is", "fun"}

## गहराई में जाएं
रेग्युलर एक्सप्रेशन्स का इस्तेमाल करना बहुत आसान है। लेकिन उनकी गहराई में जाकर उन्हें समझना बहुत महत्वपूर्ण है। इस विशेष लेख में हम इसमें दी गई अवधारणाओं को गहराई से समझेंगे।

## और देखें
[Go भाषा की विस्तृत जानकारी](https://go.dev/)
[गो भाषा में रेग्र्युलर एक्सप्रेशन्स के बारे में विस्तृत गाइड](https://golang.org/pkg/regexp/)
[पाइथन में रेग्युलर एक्सप्रेशन्स का इस्तेमाल कैसे करें](https://www.programiz.com/python-programming/regex)