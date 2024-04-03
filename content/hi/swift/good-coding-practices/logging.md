---
date: 2024-01-26 01:08:55.379886-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Swift \u092E\u0947\
  \u0902, \u0906\u092A \"print\" \u0938\u094D\u091F\u0947\u091F\u092E\u0947\u0902\u091F\
  \u094D\u0938 \u0915\u0947 \u0938\u093E\u0925 \u092F\u093E \u0905\u0927\u093F\u0915\
  \ \u0932\u091A\u0940\u0932\u0947 `os.log` API \u0915\u0947 \u0938\u093E\u0925 \u0915\
  \u0902\u0938\u094B\u0932 \u092A\u0930 \u0932\u0949\u0917 \u0932\u093F\u0916 \u0938\
  \u0915\u0924\u0947 \u0939\u0948\u0902, \u091C\u094B Apple \u092A\u094D\u0932\u0947\
  \u091F\u092B\u093C\u0949\u0930\u094D\u092E\u094D\u0938 \u092A\u0930 Unified Logging\u2026"
lastmod: '2024-03-13T22:44:52.929416-06:00'
model: gpt-4-1106-preview
summary: "Swift \u092E\u0947\u0902, \u0906\u092A \"print\" \u0938\u094D\u091F\u0947\
  \u091F\u092E\u0947\u0902\u091F\u094D\u0938 \u0915\u0947 \u0938\u093E\u0925 \u092F\
  \u093E \u0905\u0927\u093F\u0915 \u0932\u091A\u0940\u0932\u0947 `os.log` API \u0915\
  \u0947 \u0938\u093E\u0925 \u0915\u0902\u0938\u094B\u0932 \u092A\u0930 \u0932\u0949\
  \u0917 \u0932\u093F\u0916 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902, \u091C\u094B\
  \ Apple \u092A\u094D\u0932\u0947\u091F\u092B\u093C\u0949\u0930\u094D\u092E\u094D\
  \u0938 \u092A\u0930 Unified Logging System \u092E\u0947\u0902 \u091C\u0941\u0921\
  \u093C\u0924\u093E \u0939\u0948\u0964."
title: "\u0932\u0949\u0917\u093F\u0902\u0917"
weight: 17
---

## कैसे करें:
Swift में, आप "print" स्टेटमेंट्स के साथ या अधिक लचीले `os.log` API के साथ कंसोल पर लॉग लिख सकते हैं, जो Apple प्लेटफ़ॉर्म्स पर Unified Logging System में जुड़ता है।

```Swift
import os.log

let logger = OSLog(subsystem: "com.yourapp.domain", category: "network")

func fetchData() {
    // सरल print स्टेटमेंट
    print("Fetch started")
    
    // os.log का उपयोग करते हुए info-level इवेंट की लॉगिंग
    os_log(.info, log: logger, "Fetching data from API.")
    
    do {
        let data = try performNetworkRequest()
        // debug-level इवेंट की लॉगिंग
        os_log(.debug, log: logger, "Data received: %@", data.description)
    } catch {
        // error-level इवेंट की लॉगिंग
        os_log(.error, log: logger, "Failed to fetch data: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // नेटवर्क रिक्वेस्ट का अनुकरण
    return Data()
}
```

कंसोल पर ओउटपुट कुछ इस तरह दिख सकता है:

```
Fetch started
Fetching data from API.
Data received: Some data bytes...
```

त्रुटियों के लिए, यह हो सकता है:

```
Failed to fetch data: The Internet connection appears to be offline.
```

## गहराई से समझें
iOS 10 और macOS Sierra में पेश किये गए Unified Logging System के साथ Swift में लॉगिंग नई शक्ति और कार्यक्षमता लेकर आती है। `print` स्टेटमेंट के विपरीत जो सीधे कंसोल पर जाता है, यह सिस्टम गतिविधि-आधारित है, और आपको उनके महत्व और वे डीबग या रिलीज़ बिल्ड्स हैं इस आधार पर लॉग संदेशों को फ़िल्टर करने की अनुमति देता है।

ऐतिहासिक संदर्भ iOS और macOS में लॉगिंग के विकास को सामान्य print स्टेटमेंट्स से इंस्ट्रुमेंट्स एप्प और कंसोल के साथ एकीकृत करने वाले व्यापक उपकरणों की ओर दिखाता है, जो सोफ़िस्टिकेट तरीकों से लॉग्स का विश्लेषण करने के लिए प्रदान करते हैं।

Swift में लॉगिंग के विकल्पों के एक श्रेणी मौजूद हैं, जैसे कि CocoaLumberjack जैसे थर्ड-पार्टी लाइब्रेरियां, जो Unified Logging System के ऊपर एक मैक्रो लेयर प्रदान करती हैं। यह लॉग फार्मेटिंग, फाइल प्रबंधन, और प्रदर्शन विकल्पों पर बढ़ा हुआ नियंत्रण प्रदान करती है।

अंत में, कार्यान्वयन विवरण; OSLog का डिजाईन न केवल कार्यक्षमता के लिए है, बल्कि गोपनीयता के प्रति सचेत भी है, लॉगिंग के समय निजी डेटा को धुंधला करने की क्षमता के साथ। यह लॉग्स को फॉल्ट, त्रुटि, जानकारी, और डीबग स्तरों में वर्गीकृत करता है, प्रत्येक ट्रबलशूटिंग के लिए एक अलग अनाज की पेशकश करता है।

## यह भी देखें
- [Apple का Unified Logging दस्तावेज़ीकरण](https://developer.apple.com/documentation/os/logging)
- [Ray Wenderlich का Logging ट्यूटोरियल](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [CocoaLumberjack GitHub रिपॉजिटरी](https://github.com/CocoaLumberjack/CocoaLumberjack)
