---
date: 2024-01-20 17:32:37.427767-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Swift \u092E\u0947\
  \u0902 \u0939\u092E `Date` \u0914\u0930 `Calendar` \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930\u0915\u0947 \u0921\u0947\u091F \u0915\u094B \u092E\u0948\
  \u0928\u093F\u092A\u0941\u0932\u0947\u091F \u0915\u0930 \u0938\u0915\u0924\u0947\
  \ \u0939\u0948\u0902\u0964 \u092F\u0947 \u0930\u0939\u0947 \u0915\u094B\u0921 \u0909\
  \u0926\u093E\u0939\u0930\u0923."
lastmod: '2024-03-13T22:44:52.941391-06:00'
model: gpt-4-1106-preview
summary: "Swift \u092E\u0947\u0902 \u0939\u092E `Date` \u0914\u0930 `Calendar` \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0921\u0947\u091F\
  \ \u0915\u094B \u092E\u0948\u0928\u093F\u092A\u0941\u0932\u0947\u091F \u0915\u0930\
  \ \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\u0947 \u0930\u0939\u0947\
  \ \u0915\u094B\u0921 \u0909\u0926\u093E\u0939\u0930\u0923."
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
weight: 26
---

## कैसे करें:
Swift में हम `Date` और `Calendar` का उपयोग करके डेट को मैनिपुलेट कर सकते हैं। ये रहे कोड उदाहरण:

```Swift
import Foundation

// वर्तमान तारीख
let today = Date()

// कैलेंडर इनिशियलाइज़ करना
let calendar = Calendar.current

// भविष्य में एक सप्ताह जोड़ना
if let nextWeek = calendar.date(byAdding: .day, value: 7, to: today) {
    print("अगला सप्ताह: \(nextWeek)")
}

// अतीत में एक महीना घटाना
if let lastMonth = calendar.date(byAdding: .month, value: -1, to: today) {
    print("पिछला महीना: \(lastMonth)")
}
```

सैंपल आउटपुट:

```
अगला सप्ताह: <भविष्य की तारीख>
पिछला महीना: <अतीत की तारीख>
```

## गहराई से जानकारी:
स्विफ्ट में तारीख की गणना Foundation फ्रेमवर्क की `Date` और `Calendar` क्लासेज की मदद से की जाती है। `Date` विशिष्ट टाइमस्टैम्प को रिप्रेज़ेंट करता है, जबकि `Calendar` में समय और तारीख संबंधित गणनाएँ और मैनिपुलेशन होते हैं। इतिहास में, प्रोग्रामर्स ने मैन्युअली सेकंड्स जोड़कर तारीख की गणना की थी, जो लीप ईयर्स और विभिन्न महीनों के दिनों को देखते हुए जटिल हो सकता था। Swift ने इसे `Calendar` मेथड्स के साथ सरल बनाया है। 

विकल्पों के रूप में, DateComponents का उपयोग करके भी डेट मैनिपुलेशन संभव है, और थर्ड-पार्टी लाइब्रेरीज जैसे कि SwiftDate और DateTools भी हैं, जो और भी ज्यादा सुविधाएँ प्रदान करती हैं।

## देखें भी:
- [Apple Developer Documentation: Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Developer Documentation: Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [SwiftDate GitHub Repository](https://github.com/malcommac/SwiftDate)
- [DateTools GitHub Repository](https://github.com/MatthewYork/DateTools)
