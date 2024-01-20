---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

स्ट्रिंग इंटरपोलेशन (String Interpolation) Swift में एक तरीका है जिसका उपयोग हम किसी वैरिएबल, कॉन्स्टेंट या अन्य एक्स्प्रेशन के माध्यम से स्ट्रिंग के अंदर कुछ जोड़ने के लिए करते हैं। यह कार्यक्रमकर्ता के लिए संवाद और जानकारी साझा करने का एक आसान तरीका है। 

## कैसे करें: (How to)

आइए स्विफ्ट में string interpolation का उपयोग कैसे करें, देखते हैं:

```Swift
let name = "Rajesh"
let age = 25
let greeting = "नमस्ते, मेरा नाम \(name) है और मैं \(age) साल का हूँ।"
print(greeting)
```

और जब आप इसे रन करेंगे, यह उत्तर होगा:

```Swift
नमस्ते, मेरा नाम Rajesh है और मैं 25 साल का हूँ।
```

## गहराई में जानें (Deep Dive)

String Interpolation का विधान Swift भाषा के साथ ही शुरुआत में किया गया था। यह प्रोग्रामरों को मानव पठनीय स्ट्रिंग्स बनाने में राहत देता था, जिसमें वरिएबल्स और एक्स्प्रेशन्स शामिल थे। 

वैकल्पिक रूप से, आप "+ ऑपरेटर" का उपयोग कर सकते हैं स्ट्रिंग्स को जोड़ने के लिए, लेकिन यह Interpolation की तरह स्पष्ट और सीधे नहीं होता है।

## देखने का भी: (See Also)

अधिक जानकारी के लिए, आपको निम्नलिखित स्रोतों पर जाना चाहिए:

1. [Swift Documentation: String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
3. [Hacking with Swift: String Interpolation](https://www.hackingwithswift.com/read/0/5/string-interpolation)