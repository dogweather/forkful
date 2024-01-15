---
title:                "वास्तविक अभिव्यक्तियों का उपयोग"
html_title:           "Swift: वास्तविक अभिव्यक्तियों का उपयोग"
simple_title:         "वास्तविक अभिव्यक्तियों का उपयोग"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

Regular Expressions (नियमित अभिव्यक्ति) स्ट्रिंग (शब्दांश) पर संधियों (patterns) का आपादन (implementation) करने का एक शक्तिशाली तरीका है। यह कोडिंग को सुगम बनाता है और खुशामद को बढ़ाता है, साथ ही एप्लिकेशन के संचालन (functionality) को भी बढ़ाता है।

## कैसे करें

Regular Expressions को Swift में कैसे इस्तेमाल किया जाए, उसे समझने के लिए हम एक सामान्य संदर्भ (context) देखेंगे। उसके बाद मैं आपको दो उदाहरण दिखाऊंगा जो कि आपको कोडिंग में Regular Expressions का इस्तेमाल करने में मदद करेंगे।

उदाहरण 1: प्रथम, हमें एक रेगुलर एक्सप्रेशन बनाने की आवश्यकता होगी। यह हमारे पास AlphaNumeric (अल्फान्यूमेरिक) शब्द को खोजने की कोशिश करेगा। इसके लिए हम "```Swift 
let regex = try! NSRegularExpression(pattern: "[A-Za-z]+[0-9]+[A-Za-z]+") 
```" कोड का इस्तेमाल करेंगे।

अब, हमें एक String (शब्दांश) को पावर करने के लिए आवश्यक होगा। हम इसे "```Swift
let string = "abc123def" 
```" का इस्तेमाल करेंगे।

आखिरकार, हमारे पहले उदाहरण में हमें code को Match करने की आवश्यकता होगी। हम "```Swift
let match = regex.matches(in: string, range: NSRange(fromString: string)) 
```" का उपयोग करेंगे।इसे रन करने पर हमे Output मिलेगा "```Swift
[
NSRange(location: 0, length: 6)
] 
```"

उदाहरण 2: दूसरे उदाहरण में, हमारे पास एक String (शब्दांश) है जो कि अनुमति (permission) के शर्तों को जाँचता है। इसको हमारे Regular Expression "```Swift 
let regex = try! NSRegularExpression(pattern: "^[A-Za-z0-9_-]*$") 
```" द्वारा जांच सकते ह