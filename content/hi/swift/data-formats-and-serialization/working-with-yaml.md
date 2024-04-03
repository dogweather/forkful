---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:48.930190-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Swift \u092E\u0947\
  \u0902 YAML \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0914\u0930 \u0938\
  \u0940\u0930\u093F\u092F\u0932\u093E\u0907\u091C\u0947\u0936\u0928 \u0915\u0947\
  \ \u0932\u093F\u090F \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u0938\u092A\u094B\
  \u0930\u094D\u091F \u0936\u093E\u092E\u093F\u0932 \u0928\u0939\u0940\u0902 \u0939\
  \u0948, \u0907\u0938\u0932\u093F\u090F \u0924\u0940\u0938\u0930\u0947 \u092A\u0915\
  \u094D\u0937 \u0915\u0940 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940\
  \u091C \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0906\u0935\u0936\u094D\u092F\
  \u0915\u0924\u093E \u0939\u094B\u0924\u0940 \u0939\u0948\u0964 \u090F\u0915\u2026"
lastmod: '2024-03-13T22:44:52.953323-06:00'
model: gpt-4-0125-preview
summary: "Swift \u092E\u0947\u0902 YAML \u092A\u093E\u0930\u094D\u0938\u093F\u0902\
  \u0917 \u0914\u0930 \u0938\u0940\u0930\u093F\u092F\u0932\u093E\u0907\u091C\u0947\
  \u0936\u0928 \u0915\u0947 \u0932\u093F\u090F \u092C\u093F\u0932\u094D\u091F-\u0907\
  \u0928 \u0938\u092A\u094B\u0930\u094D\u091F \u0936\u093E\u092E\u093F\u0932 \u0928\
  \u0939\u0940\u0902 \u0939\u0948, \u0907\u0938\u0932\u093F\u090F \u0924\u0940\u0938\
  \u0930\u0947 \u092A\u0915\u094D\u0937 \u0915\u0940 \u0932\u093E\u0907\u092C\u094D\
  \u0930\u0947\u0930\u0940\u091C \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0906\
  \u0935\u0936\u094D\u092F\u0915\u0924\u093E \u0939\u094B\u0924\u0940 \u0939\u0948\
  \u0964 \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0935\u093F\
  \u0915\u0932\u094D\u092A `Yams` \u0939\u0948, \u091C\u094B Swift \u092E\u0947\u0902\
  \ YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F \u090F\u0915 \u092A\u0941\u0938\u094D\u0924\u0915\
  \u093E\u0932\u092F \u0939\u0948\u0964\n\n\u092A\u0939\u0932\u0947, \u0906\u092A\u0915\
  \u094B \u0905\u092A\u0928\u0947 \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\
  \u091F \u092E\u0947\u0902 `Yams` \u091C\u094B\u0921\u093C\u0928\u0947 \u0915\u0940\
  \ \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E \u0939\u0948\u0964 \u092F\u0926\
  \u093F \u0906\u092A Swift Package Manager \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930 \u0930\u0939\u0947 \u0939\u0948\u0902, \u0924\u094B \u0906\u092A\
  \ \u0907\u0938\u0947 \u0905\u092A\u0928\u0940 `Package.swift` \u092B\u093C\u093E\
  \u0907\u0932 \u092E\u0947\u0902 \u090F\u0915 \u0928\u093F\u0930\u094D\u092D\u0930\
  \u0924\u093E \u0915\u0947 \u0930\u0942\u092A \u092E\u0947\u0902 \u091C\u094B\u0921\
  \u093C \u0938\u0915\u0924\u0947 \u0939\u0948\u0902."
title: "YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 41
---

## कैसे करें:
Swift में YAML पार्सिंग और सीरियलाइजेशन के लिए बिल्ट-इन सपोर्ट शामिल नहीं है, इसलिए तीसरे पक्ष की लाइब्रेरीज का उपयोग आवश्यकता होती है। एक लोकप्रिय विकल्प `Yams` है, जो Swift में YAML के साथ काम करने के लिए एक पुस्तकालय है।

पहले, आपको अपने प्रोजेक्ट में `Yams` जोड़ने की आवश्यकता है। यदि आप Swift Package Manager का उपयोग कर रहे हैं, तो आप इसे अपनी `Package.swift` फ़ाइल में एक निर्भरता के रूप में जोड़ सकते हैं:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

### Swift में YAML पार्स करना
मान लें कि आपके पास एक सरल ऐप के लिए निम्नलिखित YAML कॉन्फ़िगरेशन है:

```yaml
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
```

`Yams` का उपयोग करके Swift में इस YAML स्ट्रिंग को कैसे पार्स करें, यहाँ देखें:

```swift
import Yams

let yamlString = """
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
        // पार्स किए गए डेटा तक पहुँचने का उदाहरण
        if let name = data["name"] as? String {
            print("App Name: \(name)")
        }
    }
} catch {
    print("YAML पार्स करने में त्रुटि: \(error)")
}
```

नमूना आउटपुट:

```
["name": MyApp, "version": 1.0, "environment": "development", "features": ["login", "notifications"]]
App Name: MyApp
```

### Swift ऑब्जेक्ट्स को YAML में सीरियलाइज करना
`Yams` के साथ Swift ऑब्जेक्ट को वापस एक YAML स्ट्रिंग में परिवर्तित करना भी सीधा है। मान लें आपके पास वही डेटा संरचना है जिसे सीरियलाइज किया जाना है:

```swift
let appInfo = [
    "name": "MyApp",
    "version": 1.0,
    "environment": "development",
    "features": ["login", "notifications"]
] as [String : Any]

do {
    let yamlString = try Yams.dump(object: appInfo)
    print(yamlString)
} catch {
    print("YAML में सीरियलाइज करने में त्रुटि: \(error)")
}
```

यह एक YAML-फॉर्मेटेड स्ट्रिंग उत्पन्न करेगा:

```yaml
environment: development
features:
  - login
  - notifications
name: MyApp
version: 1.0
```

ये उदाहरण Swift एप्लिकेशन्स में YAML के साथ काम करने के बुनियादी ऑपरेशन दिखाते हैं। याद रखें, जबकि YAML मानव पढ़ाई और उपयोग में आसानी में उत्कृष्ट है, अपने एप्लिकेशन की विशिष्ट आवश्यकताओं का विशेष रूप से प्रदर्शन और जटिलता के संबंध में चुनाव करते समय हमेशा ध्यान में रखें, जब आप अपना डेटा सीरियलाइजेशन प्रारूप चुनें।
