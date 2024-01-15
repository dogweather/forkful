---
title:                "स्ट्रिंगों का सम्मिलन"
html_title:           "Go: स्ट्रिंगों का सम्मिलन"
simple_title:         "स्ट्रिंगों का सम्मिलन"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों

जैसा कि सभी विकासकों को पता है, प्रोग्रामिंग में स्ट्रिंग्स का बादलिंग (concatenation) काफी उपयोगी होती है। अधिकतर उपयोग होने वाले स्थानों में, हमें स्ट्रिंग्स को एक साथ जोड़ना पड़ता है, जैसे कि विभिन्न पाठ स्ट्रिंग्स को एक बार में और बहुत अधिक संख्या में केवल एक स्थान पर प्रिंट करने के लिए। इसलिए, इस लेख में हम आपको स्ट्रिंग्स की बादलिंग का सरल तरीका बताएंगे जो कि Go भाषा में बहुत ही आसान है।

## कैसे करें

```Go
package main

import "fmt"

func main() {
	// सरल स्ट्रिंग्स का बादलिंग
	firstName := "विवेक"
	lastName := "शर्मा"
	fullName := firstName + " " + lastName
	fmt.Println(fullName)

	// पाठ स्ट्रिंग्स का बादलिंग
	text1 := "Hello"
	text2 := "world!"
	text3 := "This is a"
	text4 := "concatenated string."
	concatenatedText := text1 + " " + text2 + ", " + text3 + " " + text4
	fmt.Println(concatenatedText)
}
```

**आउटपुट:**

```
विवेक शर्मा
Hello world!, This is a concatenated string.
```

इस उदाहरण में, हमने स्ट्रिंग्स को एक-दूसरे के साथ जोड़ा एवं उन्हें प्रिंट किया है। पहले उदाहरण में, आप देख सकते हैं कि हमने पाठ स्ट्रिंग्स को सीधे जोड़ा है और दूसरे उदाहरण में हमने पहले स्थान पर पहले चरित्र को रखा है जो जोड़ने के लिए खाली स्ट्रिंग युक्त होता है। यह एक ऐसी कला है जो आप आसानी से सीख सकते हैं और इसका उपयोग कर सकते हैं।

## गहराई में जाएं

जब हम गो में स्ट्रिंग्स को बादलते हैं, तो स्ट्रिंग वेरिएबल्स (variables) को