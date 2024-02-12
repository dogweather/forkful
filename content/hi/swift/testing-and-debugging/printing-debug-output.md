---
title:                "डीबग आउटपुट प्रिंट करना"
aliases:
- /hi/swift/printing-debug-output/
date:                  2024-01-20T17:54:22.357355-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डिबग आउटपुट प्रिंट करना मतलब अपने कोड के बीच में अस्थायी मैसेज दिखाना जिससे पता चलता है कि प्रोग्राम कैसे चल रहा है। प्रोग्रामर्स यह इसलिए करते हैं क्योंकि इससे बग्स ढूंढने और कोड को समझने में आसानी होती है।

## How to: (कैसे करें:)
Swift में डिबग स्टेटमेंट्स डालने के लिए `print()` फंक्शन का इस्तेमाल होता है। यहां कुछ उदाहरण हैं:

```Swift
// सिंपल मैसेज प्रिंट करना
print("Hello, debug!")

// वैरिएबल के साथ मैसेज प्रिंट करना
var score = 42
print("Your score is \(score).")

// कंडीशनल डिबगिंग
var isDebugMode = true
if isDebugMode {
    print("Debug mode is enabled.")
}
```

सैंपल आउटपुट:

```
Hello, debug!
Your score is 42.
Debug mode is enabled.
```

## Deep Dive (गहराई से जानकारी)
पहले, लॉग स्टेटमेंट्स का इस्तेमाल टर्मिनल और कंसोल्स में डिबग संदेशों को देखने के लिए किया जाता था। Swift में `print()` सबसे आम तरीका है, पर इसके अलावा `debugPrint()` और `dump()` जैसे फंक्शंस भी हैं जो अधिक विस्तृत जानकारी प्रदान करते हैं।

जब `print()` बहुत साधारण मैसेज प्रिंट करता है, `debugPrint()` डेवेलपमेंट के समय में अधिक तकनीकी विवरण दिखाता है, जैसे कि इंस्टेंस का डिबग डिस्क्रिप्शन। `dump()` फंक्शन ऑब्जेक्ट की संरचना को भी दिखाता है, जो कि जटिल डेटा संरचनाओं को डिबग करने में उपयोगी होता है।

हालांकि, बड़े प्रोजेक्ट्स में `print()` को बहुत इस्तेमाल करने से लॉग फाइल्स गड़बड़ हो सकती हैं। इसके लिए, लॉगिंग फ्रेमवर्क जैसे कि Apple का `os_log` उपयोगी हैं, जो विस्तृत और परतदार लॉगिंग प्रदान करते हैं।

## See Also (और जानकारी के लिए)
- [Swift.org - The Basics](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- Apple Developer - [Logging](https://developer.apple.com/documentation/os/logging)
- [Ray Wenderlich - Swift Logging Ultimate Guide](https://www.raywenderlich.com/605079-logging-in-swift-ultimate-guide)
