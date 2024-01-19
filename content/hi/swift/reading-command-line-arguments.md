---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वितरक रेखा युक्तियों का पठन एक गतिविधि है जिसमें प्रोग्राम को उसके स्टार्टअप द्वारा दिए गए विचारण, के रूप में inputs मिलते हैं। प्रोग्रामर्स इसे उपयोग करते हैं ताकि प्रयोगकर्ताओं को उनकी ऐप में व्यक्तिगत वरियताओं के साथ इंटरेक्ट करने की अनुमति हो।

## कैसे करें

नीचे दिए गए स्विफ्ट कोड स्निपेट में हम Command Line Arguments को पढ़ेंगे:

```swift
let commandLineArgs = CommandLine.arguments

for arg in commandLineArgs {
    print("Argument: \(arg)")
}
```

जब आप ऊपर का कोड चलाएंगे, तो Swift प्रत्येक Command Line Argument को प्रिंट करेगा।

## गहराई से समझना

Command Line Arguments का उपयोग करने का विचार Unix तथा Linux operating systems के प्रथम प्रयोग से आया। उन्होंने इस शैली को बनाया ताकि उपयोगकर्ता कोई भी program decided parameters के साथ चला सके। इस तरह का परामर्श आज भी लागु होता है।

तापसीयाओं का और एक विकल्प हो सकता है फ़ाइल पढ़ने और प्रयोगकर्ता से डेटा प्रेसनलाइज़ड इनपुट के तौर पर लेने का उपयोग करना। हालाँकि, Command Line Arguments अधिक फ्लेक्सिबल और त्वरित तरीका प्रदान करता है, खासकर जब ऐप्लिकेशन को बार-बार दोहराया जाना हो।

इम्प्लीमेंटेशन की विवरण की बात की जाए तो, `CommandLine.arguments` एक ही स्थलीय विचारण सूची प्रदान करता है। 

## अन्य लिंक

1. [Swift Language Guide's Section on Command line arguments](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
2. [Basic Command Line Scripting in Swift](https://www.swiftbysundell.com/basics/command-line-scripting/)
3. [Handle command line arguments in a Swift script](https://augmentedcode.io/2019/04/07/handle-command-line-arguments-in-a-swift-script/)