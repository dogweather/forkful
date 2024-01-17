---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Swift: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डीबग आउटपुट प्रिंट करना एक प्रोग्रामरों के लिए बहुत ही महत्वपूर्ण है। यह उनको अपनी कोड में संशोधन करने के लिए आवश्यक जानकारी प्रदान करता है। डीबग आउटपुट से हम अपने कोड की समस्याओं को खोज और उन्हें ठीक कर सकते हैं।

## कैसे करें?

 ```Swift
 print("Hello, world!")
 // Output: Hello, world!
 
 var num1 = 5
 var num2 = 3
 print(num1 + num2)
 // Output: 8
 
 let str = "Debugging is crucial!"
 print("Don't forget to \(str)")
 // Output: Don't forget to Debugging is crucial!
 ```
डीबग आउटपुट करने के लिए हम `print()` फंक्शन का उपयोग करते हैं। यह फंक्शन हमारे द्वारा दिए गए आर्ग्यूमेंट को कंसोल पर प्रिंट करता है। हम इस फंक्शन के अंदर किसी भी डेटा टाइप को पास कर सकते हैं, जैसे स्ट्रिंग्स, इंटीजर्स या वेरिएबल्स।

## गहराई में जाएं

डीबग आउटपुट प्रिंट करने की खोज बहुत पुरानी है। पहले प्रोग्रामिंग भाषाओं में, यह आधिकारिक तौर पर `printf()` फंक्शन का उपयोग किया जाता था। आज, अधिकांश प्रोग्रामिंग भाषाओं में इसका एक स्वतन्त्र फंक्शन होता है। अन्य विकल्पों के रूप में, हम डीबग लग कर कोड को दौड़ा सकते हैं या निर्माता उत्पादन (debug vs production) सेटिंग्स में अंतरिक्ष दिखा सकते हैं। इम्प्लिमेंटेशन के बारे में, `print()` फंक्शन लगभग लगभग सभी प्रोग्रामिंग भाषाओं में उपलब्ध होता है।

## देखें भी

डीबग आउटपुट के बारे में अधिक जानने के लिए, निम्न लिंक देखें:

- [Apple: Debugging in Swift](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Debugging.html)
- [Swift Blog: Debugging with Print Statements](https://swift.org/blog/debugging-with-print-statements/)
- [Medium: Debugging in Swift with Xcode](https://medium.com/@kurtulus/1-debugging-in-swift-with-xcode-67bb2eda5808)