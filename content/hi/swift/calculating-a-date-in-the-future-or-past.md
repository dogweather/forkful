---
title:                "भविष्य या अतीत में तारीख की गणना"
date:                  2024-01-20T17:32:37.427767-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

भविष्य या अतीत में तारीख की गणना का मतलब है आज की तारीख से कुछ दिन, महीने या साल बढ़ाना या घटाना। प्रोग्रामर इसका उपयोग इवेंट्स, रिमाइंडर्स, और डेटा एनालिसिस के लिए करते हैं।

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
