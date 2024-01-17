---
title:                "HTML को पार्स करना"
html_title:           "Swift: HTML को पार्स करना"
simple_title:         "HTML को पार्स करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/parsing-html.md"
---

{{< edit_this_page >}}

## क्या & क्यों?
पार्सिंग HTML क्या है और क्यों प्रोग्रामर इसे करते हैं? HTML पृष्ठों को कंप्यूटर को समझने और प्रसंस्करण के लिए संरचित डेटा में रूपांतरित करने के लिए उपयोग किया जाता है। प्यारोसिंग HTML से, हम वेब पृष्ठों से जानकारी प्राप्त कर सकते हैं और उसे अपनी आवश्यकताओं के अनुसार समायोजित कर सकते हैं।

## कैसे:
```Swift
let html = "<html><head><title>Swift HTML Parsing</title></head><body><h1>Welcome to Swift</h1><p>This is a tutorial on HTML parsing using Swift programming language.</p></body></html>"
let parser = HTMLParser()
let parsedHTML = parser.parse(html: html)
print(parsedHTML)
```
आपको परिणाम में निम्नलिखित मिलेंगे:

```
[<html>, <head>, <title>, Swift HTML Parsing, </title>, </head>, <body>, <h1>, Welcome to Swift, </h1>, <p>, This is a tutorial on HTML parsing using Swift programming language., </p>, </body>, </html>]
```

## गहराई में खुदी:
पार्सिंग HTML को सबसे पहले 1993 में टिम बर्नर्स-ली के द्वारा विकसित किया गया था। आजकल, कई भाषाओं और टूल्स को HTML पार्स करने के लिए उपयोग किया जाता है, लेकिन Swift पार्सिंग के लिए एक शक्तिशाली विकल्प है। Swift के लिए कुछ प्रमुख पार्सिंग लाइब्रेरी हैं, जैसे HTMLReader और Kanna। आप अपनी आवश्यकताओं के अनुसार किसी भी पुस्तकालय का उपयोग कर सकते हैं।

## देखें भी:
- [Apple's Official Documentation for String](https://developer.apple.com/documentation/foundation/string)
- [HTMLParser Library for Swift](https://github.com/tid-kijyun/Swift-HTML-Parser)
- [An Introduction to Parsing HTML using Swift](https://medium.com/@rzrasel/introduction-to-parsing-html-in-swift-42a0385facc5)