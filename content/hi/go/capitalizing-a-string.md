---
title:                "स्ट्रिंग कॅपिटलाइज़ करना"
html_title:           "Go: स्ट्रिंग कॅपिटलाइज़ करना"
simple_title:         "स्ट्रिंग कॅपिटलाइज़ करना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

गो प्रोग्रामिंग भाषा में स्ट्रिंग को कैपिटलाइज करना एक आम काम है जो डेवलपर्स द्वारा उनके कोड को एक्सेप्टेबल और प्रोफेशनल लुक करने के लिए किया जाता है। इसके अलावा, कई बार डेटा को सर्वर्स व मशीनों को संभोगवश भी स्ट्रिंग को कैपिटलाइज किया जाता है।

## कैसे करें

गो प्रोग्रामिंग भाषा में स्ट्रिंग को कैपिटलाइज करने के लिए, सबसे पहले हमें `strings` पैकेज को इम्पोर्ट करना होगा। फिर हम `strings.Title()` फंक्शन का उपयोग करके स्ट्रिंग को कैपिटलाइज कर सकते हैं।

इसके लिए, हम एक `string` को `strings.Title()` फंक्शन के अंदर पास करेंगे और इसका रिटर्न वैल्यू को एक नए वेरिएबल में स्टोर करेंगे। नीचे दिए गए कोड ब्लॉक में आप इसका सम्पूर्ण एक्साम्पल देख सकते हैं।

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	myString := "hello world"
	myCapitalizedString := strings.Title(myString)

	fmt.Println(myCapitalizedString)
}
```

उपरोक्त कोड का आउटपुट होगा: "Hello World"

## गहराई में जाएं

स्ट्रिंग को कैपिटलाइज करने का अधिक गहरा अध्ययन करने के लिए, आप `strings.Title()` फंक्शन के अंतर्गत क्या तरीके मौजूद हैं और यह कैसे काम करता है, इसे अधिक गहराई से समझेंगे। इसके अलावा, आप `unicode` पैकेज का उपयोग करके भी स्ट्रिंग को कैपिटलाइज कर सकते हैं।

## आगे देखें

- [Go स्ट्रिंग मैनिपुलेशन गाइड](https://www.digitalocean.com/community/tutorials/how-to-manipulate-strings-in-go)
- [Go प्रोग्रामिं