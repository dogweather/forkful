---
title:                "स्ट्रिंग को अंतर्पोल करना"
html_title:           "Go: स्ट्रिंग को अंतर्पोल करना"
simple_title:         "स्ट्रिंग को अंतर्पोल करना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

मस्ती से लिखा गया लेख

## क्या और क्यों?

वह लोग जो कुछ भी कोड करते हैं, कभी-कभी उनके पास किसी भी स्ट्रिंग को अन्य strings के साथ मिलाना होता है। इसको बोलते हैं स्ट्रिंग को अंतर्गत दर्शाने का तरीका। इससे हमें अपने कोड को और सरल बनाने का एक अच्छा तरीका मिलता है।

## कैसे करें?

```Go
package main

import "fmt"

func main() {
	name := "आपका नाम"
	age := 20

	output := fmt.Sprintf("मेरा नाम है %s और मैं %d वर्ष का हूँ।", name, age)
	fmt.Println(output)
}
```

आपको नीचे दिए गए आउटपुट मिलेगा:

```shell
मेरा नाम है आपका नाम और मैं 20 वर्ष का हूँ।
```

## गहराई तक जाएं

इस तरीके को प्रोग्रामिंग में हमें मौजूदा स्ट्रिंग पैटर्न के साथ इंटरएक्ट करने में मदद मिलती है। यह एक सरल और एफ़िक्सिएंट तरीका है। इससे हमें एक ही बार में कई स्ट्रिंग्स को मिलाने की सुविधा मिलती है। इसके अलावा, यह अन्य प्रोग्रामिंग भाषाओं में भी इस्तेमाल किया जाता है।

## अन्य स्रोत देखें

- [Official Go Documentation on fmt package](https://golang.org/pkg/fmt/)
- [Tutorial on String Interpolation in Go](https://www.calhoun.io/string-interpolation-in-go/)