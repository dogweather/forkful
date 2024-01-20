---
title:                "स्ट्रिंग को बड़े अक्षरों में बदलना"
html_title:           "Go: स्ट्रिंग को बड़े अक्षरों में बदलना"
simple_title:         "स्ट्रिंग को बड़े अक्षरों में बदलना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग को कैपिटलाइज करना मतलब उसके पहले अक्षर को बड़े (अगर वह छोटे हो) में बदलना। प्रोग्रामर्स इसे स्वरूपानुसार अलग-थलग वाक्यांशों को पहचानने के लिए करते हैं।

## कैसे करें:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {

	myString := "यह एक संदर्भ स्ट्रिंग है।"
	
	fmt.Println(strings.Title(myString))

}
```
ऊपर का कोड स्ट्रिंग को कैपिटलाइज करता है। आउटपुट इस प्रकार होगा:

```Go
यह एक संदर्भ स्ट्रिंग है।
```

## गहराई में:

स्ट्रिंग कैपिटलाइजेशन पहलियों से ही उपयोग में रहा है, यह विशेष लक्षीयता हमें वाक्यांशों और विशिष्ट शब्दों को पहचानने में मदद करती है। जैसे की Go में: `strings.Title()` और `strings.ToTitle()` जैसे फंक्शन होते हैं। स्ट्रिंग कैपिटलाइजेशन को हांडल करने के लिए अन्य भाषाओं में भी भिन्न-भिन्न में फंक्शन्स होते हैं। 

## देखने के लिए भी:

1. [Go strings package documentation](https://golang.org/pkg/strings/)
2. [A guide to string literals in Go](https://www.digitalocean.com/community/tutorials/understanding-strings-in-go)
3. [Go by Example: String Functions](https://gobyexample.com/string-functions)