---
date: 2024-01-20 17:52:24.027404-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.894069-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
weight: 8
---

## How to: (कैसे करें:)
```Swift
// साधारण इंटरपोलेशन
let name = "रोहित"
let greeting = "नमस्ते, \(name)!"
print(greeting) // आउटपुट: नमस्ते, रोहित!

// गणना के साथ इंटरपोलेशन
let apples = 3
let oranges = 5
let fruitSummary = "मेरे पास \(apples + oranges) फल हैं।"
print(fruitSummary) // आउटपुट: मेरे पास 8 फल हैं।

// दशमलव संख्या को फॉर्मेट करना
let temperature = 36.6
let weatherMessage = "आज का तापमान \(temperature)°C है।"
print(weatherMessage) // आउटपुट: आज का तापमान 36.6°C है।
```

## Deep Dive (गहराई में जानकारी)
स्ट्रिंग इंटरपोलेशन Swift भाषा में शुरुआत से ही था। इससे पहले, C लैंग्वेज में `printf` या Objective-C में `NSString` के `stringWithFormat:` का इस्तेमाल होता था। Swift ने इसे आसान बनाया है, साफ़ सिंटैक्स के साथ।

जब आप स्ट्रिंग में इंटरपोलेशन का इस्तेमाल करते हैं, Swift कंपाइलर उसे `String` के `init(stringInterpolation:)` इनिशियलाइजर का इस्तेमाल करके बनाता है। आप कस्टम `StringInterpolation` बना सकते हैं अगर आप अपनी स्ट्रिंग्स को और भी अनुकूलित करना चाहते हों।

वैकल्पिक तरीकों में `+` ऑपरेटर, स्ट्रिंग्स के लिए आर्रे का इस्तेमाल या `String` फंक्शन जैसे `appending(_:)` हैं, लेकिन ये इंटरपोलेशन से ज्यादा कोड लिखना पड़ता है और साफ सिंटैक्स नहीं होता।

## See Also (अधिक जानकारी के लिए)
- [The Swift Programming Language - String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
