---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

Command line arguments वे मुद्दे हैं जो किसी प्रोग्राम को उसके प्रारंभ करने के समय पास किए जाते हैं। कार्यकर्ता इनका उपयोग अक्सर उस प्रोग्राम की कार्यक्षमता को बदलने के लिए करते हैं।

## कैसे करें:

Go में, आप `os` पैकेज का उपयोग करके command line arguments को पढ़ सकते हैं। 

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	argsWithProg := os.Args
	argsWithoutProg := os.Args[1:]

	arg := os.Args[3]

	fmt.Println(argsWithProg)
	fmt.Println(argsWithoutProg)
	fmt.Println(arg)
}
```
इसे `go run main.go arg1 arg2 arg3` के रूप में चलाएँ और आपको निम्नलिखित आउटपुट मिलेगा 

```
[main arg1 arg2 arg3]
[arg1 arg2 arg3]
arg3
``` 
## गहरा डाइव:

तरीखी प्रसंग में, command line arguments का उपयोग UNIX ऑपरेटिंग सिस्टम से शुरू हुआ था, जहां उन्होंने प्रोग्राम्स को उनकी विशेषताओं को तब्दील करने की अनुमति दी। Go में, इन्हें `os.Args` के माध्यम से प्राप्त किया जाता है, जो एक string की सूची होती है। 

वैकल्पिक रूप से, यदि आपको अधिक नियंत्रण की आवश्यकता है, तो `flag` पैकेज का उपयोग कर सकते हैं जो अधिक उन्नत command line parsing प्रदान करता है। 

## और जानिए:

अधिक जानकारी के लिए, निम्नलिखित कड़ियाँ देखें:

- Go द्वारा एक गाइड जो विवरण देता है कि कैसे command line arguments को पढ़ें: https://gobyexample.com/command-line-arguments
- `flag` पैकेज द्वारा प्रदान किए गए मजबूत command line parsing की जानकारी के लिए देखें: https://golang.org/pkg/flag/