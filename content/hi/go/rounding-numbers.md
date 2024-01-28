---
title:                "संख्याओं को पूर्णांक बनाना"
date:                  2024-01-26T03:46:53.210793-07:00
model:                 gpt-4-0125-preview
simple_title:         "संख्याओं को पूर्णांक बनाना"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
संख्याओं को गोल करने का मतलब है किसी संख्या को उसके निकटतम पूर्णांक या निर्दिष्ट दशमलव स्थान तक संशोधित करना। यह मूल्यों को सरल बनाने, उन्हें अधिक पठनीय बनाने, या कुछ निश्चित प्रतिबंधों में फिट करने के लिए किया जाता है, जैसे कि मुद्राओं के साथ काम करते समय।

## कैसे:
गो का `math` पैकेज आपका दोस्त है जब संख्याओं को गोल करने की बात आती है। सरलता के लिए `math.Round`, `math.Floor`, और `math.Ceil` का उपयोग करें:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // सबसे नज़दीकी पूर्णांक में गोल करें
	fmt.Println("Floor:", math.Floor(number)) // नीचे की ओर गोल करें
	fmt.Println("Ceil: ", math.Ceil(number))  // ऊपर की ओर गोल करें
}
```

नमूना आउटपुट:
```
Round: 3
Floor: 3
Ceil: 4
```

विशेष दशमलव स्थानों के लिए, गुणा करें, गोल करें, फिर विभाजित करें:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("2 दशमलव स्थानों पर गोल:", roundToDecimalPlace(number, 2))
}
```

नमूना आउटपुट:
```
2 दशमलव स्थानों पर गोल: 3.14
```

## गहराई से जानकारी
संख्याओं को गोल करना नया नहीं है—यह प्राचीन गणित के समय से है, हमेशा सरलता के लिए लक्ष्यित है। गो में `math.Round` [बैंकर्स' गोल करना](https://en.wikipedia.org/wiki/Rounding#Round_half_to_even) का उपयोग करता है, जिसका अर्थ है कि 0.5 सबसे नज़दीकी सम संख्या में गोल होती है, यह कुछ योगों पर प्रभाव डाल सकने वाले पूर्वाग्रह को कम करता है।

फ्लोटिंग-पॉइंट संख्याएँ उनकी बाइनरी प्रतिनिधित्व के कारण चालाक हो सकती हैं, जो सभी दशमलवों को सटीक रूप से प्रस्तुत नहीं कर सकती हैं। हालांकि, गो का दृष्टिकोण अधिकांश समय अपेक्षित व्यवहार को बनाए रखता है।

अन्य गोल करने के तरीके मौजूद हैं, जैसे "आधा ऊपर गोल करें" या "आधा शून्य से दूर गोल करें," लेकिन गो की मानक लाइब्रेरी वह है जो तत्काल उपलब्ध है। अधिक जटिल जरूरतों के लिए, आपको एक तृतीय-पक्ष लाइब्रेरी की आवश्यकता हो सकती है या अपना समाधान बनाना पड़ सकता है।

## देखें भी
- गो का `math` पैकेज: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- फ्लोटिंग-पॉइंट अंकगणित के लिए IEEE 754 मानक (फ्लोट्स को संभालने के लिए गो का आधार): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- फ्लोटिंग-पॉइंट को समझना: ["What Every Computer Scientist Should Know About Floating-Point Arithmetic"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
