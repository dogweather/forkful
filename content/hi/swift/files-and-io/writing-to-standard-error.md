---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:35:40.592458-07:00
description: "\u0915\u0948\u0938\u0947: Swift \u092E\u0947\u0902, \u092E\u093E\u0928\
  \u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u092E\u0947\u0902 \u0932\u093F\u0916\
  \u0928\u093E \u0938\u0940\u0927\u0947 stderr \u0924\u0915 \u092A\u0939\u0941\u0901\
  \u091A \u0915\u0947 \u0932\u093F\u090F `FileHandle` \u0915\u094D\u0932\u093E\u0938\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0915\u093F\
  \u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\
  \u093E\u0901 \u090F\u0915 \u0938\u093E\u0927\u093E\u0930\u0923 \u0909\u0926\u093E\
  \u0939\u0930\u0923 \u0939\u0948."
lastmod: '2024-03-13T22:44:52.946568-06:00'
model: gpt-4-0125-preview
summary: "Swift \u092E\u0947\u0902, \u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\
  \u091F\u093F \u092E\u0947\u0902 \u0932\u093F\u0916\u0928\u093E \u0938\u0940\u0927\
  \u0947 stderr \u0924\u0915 \u092A\u0939\u0941\u0901\u091A \u0915\u0947 \u0932\u093F\
  \u090F `FileHandle` \u0915\u094D\u0932\u093E\u0938 \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930\u0915\u0947 \u0915\u093F\u092F\u093E \u091C\u093E \u0938\
  \u0915\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u0938\
  \u093E\u0927\u093E\u0930\u0923 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948\
  ."
title: "\u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0915\u0947\
  \ \u0932\u093F\u090F \u0932\u093F\u0916\u0928\u093E"
weight: 25
---

## कैसे:
Swift में, मानक त्रुटि में लिखना सीधे stderr तक पहुँच के लिए `FileHandle` क्लास का उपयोग करके किया जा सकता है। यहाँ एक साधारण उदाहरण है:

```swift
import Foundation

// एक संदेश परिभाषित करें
let errorMessage = "एक त्रुटि उत्पन्न हुई।\n"

// संदेश को डेटा में परिवर्तित करें
if let data = errorMessage.data(using: .utf8) {
    // त्रुटि संदेश को stderr में लिखें
    FileHandle.standardError.write(data)
}
```

stderr को आउटपुट (आमतौर पर एक कंसोल या टर्मिनल में देखा जाता है):
```
एक त्रुटि उत्पन्न हुई।
```

अधिक जटिल लॉगिंग के लिए या बाहरी लाइब्रेरी के साथ काम करते समय, आप **SwiftLog** जैसी तृतीय-पक्ष लाइब्रेरी का उपयोग करने पर विचार करेंगे। हालाँकि, **SwiftLog** सीधे तौर पर बॉक्स से बाहर stderr में नहीं लिखता, आप इसे प्राप्त करने के लिए एक कस्टम लॉगिंग बैकेंड को लागू कर सकते हैं। यहाँ stderr में लिखने वाले कस्टम लॉग हैंडलर को परिभाषित करने का एक सरलीकृत उदाहरण है:

पहले, `Package.swift` में अपनी परियोजना निर्भरताओं में **SwiftLog** जोड़ें:
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "YourPackageName",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "YourTargetName",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
            ]),
    ]
)
```

फिर, stderr में लिखने वाला कस्टम लॉग हैंडलर लागू करें:

```swift
import Logging
import Foundation

struct StderrLogHandler: LogHandler {
    let label: String
    
    var logLevel: Logger.Level = .info
    
    func log(level: Logger.Level, message: Logger.Message, metadata: Logger.Metadata?, source: String, file: String, function: String, line: UInt) {
        let output = "\(message)\n"
        if let data = output.data(using: .utf8) {
            FileHandle.standardError.write(data)
        }
    }
    
    subscript(metadataKey metadataKey: String) -> Logger.Metadata.Value? {
        get { return nil }
        set(newValue) { }
    }
    
    var metadata: Logger.Metadata {
        get { return [:] }
        set(newMetadata) { }
    }
}

// उपयोग
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.example.yourapp")

logger.error("यह एक त्रुटि संदेश है")
```

stderr को आउटपुट:
```
यह एक त्रुटि संदेश है
```

यह कस्टम हैंडलर आपको अपने SwiftLog त्रुटि संदेशों को सीधे मानक त्रुटि में मार्गनिर्देश करने की अनुमति देता है, जिससे आपके एप्लीकेशन द्वारा उत्पन्न अन्य लॉग संदेशों के साथ सहजता से एकीकृत हो सकते हैं।
