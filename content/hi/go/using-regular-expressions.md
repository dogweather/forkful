---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रेगुलर एक्सप्रेशन्स टेक्स्ट से पैटर्न मैच करने के लिए एक तकनीक है। प्रोग्रामर इसे इसलिए इस्तेमाल करते हैं क्योंकि यह डेटा वैलिडेशन, सर्चिंग, और टेक्स्ट मैनिपुलेशन को आसान और तेज़ बना देता है।

## How to (कैसे करें):
```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// Regex pattern: ईमेल एड्रेस मैच करें
	pattern := `\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b`
	email := "namaste@duniya.com"

	// Compile the regex
	regex, _ := regexp.Compile(pattern)

	// चेक करें कि ईमेल मैच करता है या नहीं
	matched := regex.MatchString(email)
	fmt.Printf("क्या '%s' एक वैध ईमेल है? %t\n", email, matched)
	
	// Output: क्या 'namaste@duniya.com' एक वैध ईमेल है? true
}
```

## Deep Dive (गहराई से जानकारी):
रेगुलर एक्सप्रेशन्स का इतिहास 1950 के दशक में शुरू होता है, थियोरिटिकल कंप्यूटर साइंस और फॉर्मल लैंग्वेज थियरी में। Go भाषा में `regexp` पैकेज उपयोगकर्ताओं को रेगुलर एक्सप्रेशन्स को आसानी से इम्प्लीमेंट करने देता है। विकल्पों में `strings` पैकेज के साथ साइंपल सर्च ऑपरेशन शामिल हैं, लेकिन रेगुलर एक्सप्रेशन्स अधिक शक्तिशाली और लचीले हैं। `regexp` पैकेज में 'Compile' फंक्शन कॉम्प्लेक्स पैटर्न्स को ओप्टिमाइज़ करने और बार-बार उपयोग में लाने के लिए एक `Regexp` टाइप बनाता है।

## See Also (इसके अलावा):
- Go `regexp` package documentation: [pkg.go.dev/regexp](https://pkg.go.dev/regexp)
- Regular Expressions tutorial: [Regular Expressions.info](https://www.regular-expressions.info/)
- Go by Example: Regular Expressions: [gobyexample.com/regular-expressions](https://gobyexample.com/regular-expressions)
