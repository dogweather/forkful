---
date: 2024-01-20 17:54:22.357355-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Swift \u092E\
  \u0947\u0902 \u0921\u093F\u092C\u0917 \u0938\u094D\u091F\u0947\u091F\u092E\u0947\
  \u0902\u091F\u094D\u0938 \u0921\u093E\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F `print()` \u092B\u0902\u0915\u094D\u0936\u0928 \u0915\u093E \u0907\u0938\u094D\
  \u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u092F\
  \u0939\u093E\u0902 \u0915\u0941\u091B \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\
  \u0948\u0902."
lastmod: '2024-03-13T22:44:52.922734-06:00'
model: gpt-4-1106-preview
summary: "Swift \u092E\u0947\u0902 \u0921\u093F\u092C\u0917 \u0938\u094D\u091F\u0947\
  \u091F\u092E\u0947\u0902\u091F\u094D\u0938 \u0921\u093E\u0932\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F `print()` \u092B\u0902\u0915\u094D\u0936\u0928 \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0939\
  \u0948\u0964 \u092F\u0939\u093E\u0902 \u0915\u0941\u091B \u0909\u0926\u093E\u0939\
  \u0930\u0923 \u0939\u0948\u0902."
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

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
