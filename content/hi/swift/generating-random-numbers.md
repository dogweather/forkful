---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

संगणक क्रम में आकस्मिक संख्याएँ उत्पन्न करने से आकस्मिकता बढ़ती है। जैसे- यदि आप एक गेम बना रहे हैं, तो आकस्मिक संख्याएँ का उपयोग करके आप प्रतिस्पर्धात्मकता और अनपेक्षितता बढ़ा सकते हैं। 

## कैसे?
Swift में आकस्मिक संख्याएँ उत्पन्न करने के लिए कोड:
```Swift
import Swift

let randomNum = Int.random(in: 1..<10)
print(randomNum)
```
ऊपरी कोड का उत्पन्न किया गया संख्या हो सकता है 1 से 9 के बीच में कुछ भी हो।

## गहरी जानकारी :
लिखित कोड काम कैसे करता है:
आकस्मिक संख्या उत्पन्न करने के लिए Swift पहले से संगणक द्वारा उत्पादित आकस्मिक मानों का उपयोग करता है। यह यथासंभव प्रतिअवृत्तियाँ कम करने के लिए करता है। 
विकल्प:
स्विफ्ट के अलावा Perl, Python, Ruby, Java आदि भाषाओं में भी आकस्मिक संख्याएँ उत्पन्न की जा सकती हैं। सभी भाषाओं में कुछ न कुछ विशिष्ट होता है। 
आकस्मिक संख्याओं का हिस्ट्रीय संदर्भ:
पहली आकस्मिक संख्या उत्पन्न करने की क्रमानुशासन यांत्रिक कंप्यूटर द्वारा 1940 के दशक में विकसित की गई थी। अब तक, इसके उपयोग और कार्यान्वयन में बहुत बदलाव हुए हैं।

## भी देखें:
1. [Apple Docs: Swift Random Number Functions](https://developer.apple.com/documentation/swift/int/2995648-random)
2. [StackOverflow: Generate Random Numbers Swift](https://stackoverflow.com/questions/24007129/how-does-one-generate-a-random-number-in-apples-swift-language)