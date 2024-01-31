---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
html_title:           "Bash: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेक्स्ट फाइल लिखना मतलब डेटा को सादे पाठ के रूप में फाइल में सहेजना। प्रोग्रामर लॉगिंग, डेटा संग्रहण या यूजर कॉन्फिगरेशन सेव करने के लिए ऐसा करते हैं।

## How to: (कैसे करें:)
```Go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Create("/tmp/example.txt") // फाइल बनाना
	if err != nil {
		fmt.Println(err)
		return
	}
	defer file.Close() // अंत में फाइल बंद करना याद रखें

	writer := bufio.NewWriter(file)
	_, err = writer.WriteString("नमस्ते, यह एक उदाहरण टेक्स्ट है।\n") // टेक्स्ट लिखें
	if err != nil {
		fmt.Println(err)
		return
	}
	writer.Flush() // बफ़र को फ्लश करके डेटा फाइल में भेजें
}
```
इस कोड से `/tmp/example.txt` में एक टेक्स्ट फाइल बनती है जिसमें "नमस्ते, यह एक उदाहरण टेक्स्ट है।" लिखा होता है।

## Deep Dive (गहान जानकारी)
पाठ फाइलें लिखने का इतिहास संगणक प्रणाली के आरंभिक दिनों से है। `bufio` और `io/ioutil` (Go 1.16 में `io` और `os` पैकेजेज़ में शामिल हो गया) जैसे पुस्तकालय इसे आसान बनाते हैं। इसके वैकल्पिक रूप में, आप `fmt.Fprintf` या `io.WriteString` का इस्तेमाल कर सकते हैं। इंप्लीमेंटेशन डिटेल्स में फाइल परमिशंस, एन्कोडिंग और बफरिंग शामिल हैं।

## See Also (अन्य स्रोत)
- [Go by Example](https://gobyexample.com/writing-files): फाइल्स लिखने के परिदृश्यों के उदाहरण।
- [Package bufio](https://pkg.go.dev/bufio): `bufio` की अधिक जानकारी के लिए।
- [Package os](https://pkg.go.dev/os): `os` पैकेज का इस्तेमाल करके फाइल सिस्टम के साथ कार्य करने का तरीका।
