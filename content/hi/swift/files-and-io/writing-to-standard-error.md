---
aliases:
- /hi/swift/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:35:40.592458-07:00
description: "\u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F (stderr)\
  \ \u092E\u0947\u0902 \u0932\u093F\u0916\u0928\u093E, \u0906\u092A\u0915\u0947 \u0915\
  \u093E\u0930\u094D\u092F\u0915\u094D\u0930\u092E \u0915\u0947 \u0924\u094D\u0930\
  \u0941\u091F\u093F \u0938\u0902\u0926\u0947\u0936\u094B\u0902 \u092F\u093E \u0928\
  \u093F\u0926\u093E\u0928 \u0906\u0909\u091F\u092A\u0941\u091F \u0915\u094B \u092E\
  \u093E\u0928\u0915 \u0906\u0909\u091F\u092A\u0941\u091F (stdout) \u0938\u0947 \u0905\
  \u0932\u0917 \u090F\u0915 \u092A\u0943\u0925\u0915 \u0927\u093E\u0930\u093E \u092E\
  \u0947\u0902 \u0928\u093F\u0930\u094D\u0926\u0947\u0936\u093F\u0924 \u0915\u0930\
  \u0928\u0947 \u0915\u0947 \u092C\u093E\u0930\u0947\u2026"
lastmod: 2024-02-18 23:09:03.997712
model: gpt-4-0125-preview
summary: "\u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F (stderr) \u092E\
  \u0947\u0902 \u0932\u093F\u0916\u0928\u093E, \u0906\u092A\u0915\u0947 \u0915\u093E\
  \u0930\u094D\u092F\u0915\u094D\u0930\u092E \u0915\u0947 \u0924\u094D\u0930\u0941\
  \u091F\u093F \u0938\u0902\u0926\u0947\u0936\u094B\u0902 \u092F\u093E \u0928\u093F\
  \u0926\u093E\u0928 \u0906\u0909\u091F\u092A\u0941\u091F \u0915\u094B \u092E\u093E\
  \u0928\u0915 \u0906\u0909\u091F\u092A\u0941\u091F (stdout) \u0938\u0947 \u0905\u0932\
  \u0917 \u090F\u0915 \u092A\u0943\u0925\u0915 \u0927\u093E\u0930\u093E \u092E\u0947\
  \u0902 \u0928\u093F\u0930\u094D\u0926\u0947\u0936\u093F\u0924 \u0915\u0930\u0928\
  \u0947 \u0915\u0947 \u092C\u093E\u0930\u0947\u2026"
title: "\u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0915\u0947\
  \ \u0932\u093F\u090F \u0932\u093F\u0916\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

मानक त्रुटि (stderr) में लिखना, आपके कार्यक्रम के त्रुटि संदेशों या निदान आउटपुट को मानक आउटपुट (stdout) से अलग एक पृथक धारा में निर्देशित करने के बारे में है। यह बिना मानक आउटपुट को गन्दा किए, डीबगिंग और लॉगिंग त्रुटियों के लिए महत्वपूर्ण है, जिससे विकासकर्ताओं और उपयोगकर्ताओं दोनों को कार्यक्रम की स्थिति और समस्याओं की समझ में सहायता मिलती है।

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
