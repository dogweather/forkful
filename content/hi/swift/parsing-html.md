---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/parsing-html.md"
---

{{< edit_this_page >}}

##क्या और क्यों?

HTML पार्सिंग यानी HTML कोड को पढ़ने और उसे डाटा संग्रह करने की प्रक्रिया होती है। प्रोग्रामर इसे वेब पेज के डाटा को उत्पादन करने, संरचना, और विश्लेषण के लिए उपयोग करते हैं। 

## कैसे:

तो चलिए Swift में HTML पार्स करने की प्रक्रिया देखते हैं। हम SwiftSoup लाइब्रेरी का उपयोग करेंगे, जो आपको आसानी से HTML को पार्स करने देती है। 

``` Swift
import SwiftSoup

let html = "<html><head><title>मेरा शीर्षक</title></head><body><p>यहां पाठ दर्ज करें</p></body></html>"
do {
    let doc: Document = try SwiftSoup.parse(html)
    let title: String = try doc.title()
    print("शीर्षक: ", title)
} catch Exception.Error(_, let message) {
    print(message)
} catch {
    print("error")
}
```
उपरोक्त कोड का आउटपुट होगा:

``` Swift
शीर्षक: "मेरा शीर्षक"
```

## गहराी में:

HTML पार्सिंग की आवश्यकता 1990 के दशक से ही थी, जब HTML का आविष्कार हुआ था। इसके विकल्पों में, मुख्य रूप से,()यूजर्स की XML पार्सिंग की विधि होती है। लेकिन XML पार्सर HTML को सही से पार्स नहीं कर पाते हैं। 

Swift में HTML पार्सिंग, SwiftSoup बिबलियोथेकी के साथ होती है। यह बिबलियोथेकी जावा के Jsoup बिबलियोथेकी पर आधारित है, और HTML5 के DOM में सही से ट्रांसफॉर्म करने का एक सटीक तरीका प्रदान करती है। 

## यह भी देखे:

Swift के आधिकारिक डाक्यूमेंटेशन -[Swift Website](https://swift.org/documentation/)

SwiftSoup गिटहब पेज -[GitHub](https://github.com/scinfu/SwiftSoup)

HTML के लिए W3Schools ट्यूटोरियल -[W3Schools](https://www.w3schools.com/html/default.asp)