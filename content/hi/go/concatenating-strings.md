---
title:    "Go: स्ट्रिंग्स को जोड़ना"
keywords: ["Go"]
---

{{< edit_this_page >}}

## क्यों

गो प्रोग्रामिंग भाषा में स्ट्रिंग को combine करना आसान है और ये उपयोगी है क्योंकि ये दो स्ट्रिंग का एक नया स्ट्रिंग बनाता है जो प्रोग्रामर को अपने कोड को भी अधिक लचीला बनाने में मदद करता है।

## कैसे

```Go
package main

import "fmt"

func main() {
	s1 := "नमस्ते"
	s2 := "दुनिया"
	s3 := s1 + ", " + s2
	fmt.Println(s3)
}
```

आउटपुट: "नमस्ते, दुनिया"

## गहराई तक

स्ट्रिंग को संयोजित करना गो भाषा में कई तरीकों से किया जा सकता है। स्ट्रिंग को "+" ऑपरेटर द्वारा संयोजित करना आसान है लेकिन गो भाषा में "fmt.Sprintf" फंक्शन को भी इस काम के लिए इस्तेमाल किया जा सकता है। इसके अलावा स्ट्रिंग को सम्पूर्ण मान से भी संयोजित किया जा सकता है।

## देखें भी

- [Go language documentation on strings](https://golang.org/pkg/strings/)
- [Go code examples for string concatenation](https://gobyexample.com/string-concatenation)
- [Hindi blog post on basic string operations in Go](https://medium.com/hindi/%E0%A4%B5%E0%A4%B9-%E0%A4%95%E0%A4%B0%E0%A4%A4%E0%A5%87-%E0%A4%B9%E0%A5%88%E0%A4%81-strings-%E0%A4%95%E0%A5%80-%E0%A4%95%E0%A4%BE%E0%A4%AE%E0%A4%A4%E0%A4%BF%E0%A4%95-go-%E0%A4%AA%E0%A4%B0-a9df1e19e801)