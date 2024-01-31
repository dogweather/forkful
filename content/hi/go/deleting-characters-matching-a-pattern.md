---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
date:                  2024-01-20T17:42:50.983582-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Pattern से मिलने वाले characters को हटाना एक common programming task है जिसके द्वारा हम strings से unnecessary या unwanted characters को remove करते हैं। यह data sanitization, user input validation, और फाइल प्रोसेसिंग जैसे कामों के लिए बहुत ज़रुरी है।

## How to: (कैसे करें:)
Go में characters को pattern के अनुसार हटाने के लिए `strings` package में `Replace` और `ReplaceAll` functions का प्रयोग किया जा सकता है।

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	originalString := "Hello, 123 World! This is a test 456."

	// Pattern से मिलने वाले numbers को हटायें
	cleanedString := strings.Map(func(r rune) rune {
		if '0' <= r && r <= '9' {
			return -1 // इस character को हटाएं
		}
		return r
	}, originalString)

	fmt.Println(cleanedString)
}
```

Sample Output:

```
Hello, World! This is a test .
```

## Deep Dive (गहराई में जानकारी):
किसी समय में, ऐसे कामों के लिए regular expressions (RegEx) का प्रचार था, लेकिन उनका performance cost अधिक हो सकता है। `strings.Map` function Go में इस्तेमाल किया जाता है क्योंकि यह efficient है; इससे हम एक ही pass में पूरे string के प्रत्येक character पर फंक्शन अप्लाई कर सकते हैं। इस approach को चुनते वक़्त, प्रोग्रामर को उस pattern की पहचान खुद करनी होती है जिसे हटाना है। यदि जटिल patterns को हटाना है, तब RegEx का उपयोग अधिक उपयोगी हो सकता है।

## See Also (और भी जानकारी):
- Go by Example: Strings: https://gobyexample.com/strings
- Go Doc: strings package: https://pkg.go.dev/strings
- Regular Expressions in Go: https://pkg.go.dev/regexp

इन resources के माध्यम से आप Go में strings के साथ काम करना और ज्यादा अच्छे से समझ सकते हैं और अपने कोड को और भी optimized बना सकते हैं। Regular expressions के उपयोग और उनकी complexities के बारे में भी जानकारी मिलेगी।
