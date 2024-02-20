---
date: 2024-01-20 17:45:26.770113-07:00
description: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\
  \u094B\u0921\u093F\u0902\u0917 \u092F\u093E\u0928\u0940 \u0907\u0902\u091F\u0930\
  \u0928\u0947\u091F \u0938\u0947 \u092A\u0947\u091C \u0915\u0940 \u0938\u093E\u092E\
  \u0917\u094D\u0930\u0940 \u0915\u094B \u0932\u094B\u0915\u0932 \u0938\u093F\u0938\
  \u094D\u091F\u092E \u092A\u0930 \u0938\u0939\u0947\u091C\u0928\u093E\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0921\
  \u093E\u091F\u093E \u092A\u0930\u0938\u093F\u0902\u0917, \u0938\u094D\u0915\u094D\
  \u0930\u0948\u092A\u093F\u0902\u0917 \u092F\u093E \u0911\u092B\u093C\u0932\u093E\
  \u0907\u0928 \u092A\u0922\u093C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\
  \u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
lastmod: 2024-02-19 22:05:11.947447
model: gpt-4-1106-preview
summary: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921\u093F\u0902\u0917 \u092F\u093E\u0928\u0940 \u0907\u0902\u091F\u0930\u0928\
  \u0947\u091F \u0938\u0947 \u092A\u0947\u091C \u0915\u0940 \u0938\u093E\u092E\u0917\
  \u094D\u0930\u0940 \u0915\u094B \u0932\u094B\u0915\u0932 \u0938\u093F\u0938\u094D\
  \u091F\u092E \u092A\u0930 \u0938\u0939\u0947\u091C\u0928\u093E\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0921\u093E\
  \u091F\u093E \u092A\u0930\u0938\u093F\u0902\u0917, \u0938\u094D\u0915\u094D\u0930\
  \u0948\u092A\u093F\u0902\u0917 \u092F\u093E \u0911\u092B\u093C\u0932\u093E\u0907\
  \u0928 \u092A\u0922\u093C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902\u0964"
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वेब पेज डाउनलोडिंग यानी इंटरनेट से पेज की सामग्री को लोकल सिस्टम पर सहेजना। प्रोग्रामर इसे डाटा परसिंग, स्क्रैपिंग या ऑफ़लाइन पढ़ने के लिए करते हैं।

## How to: (कैसे करें:)
```swift
import Foundation

if let url = URL(string: "https://example.com") {
    let task = URLSession.shared.dataTask(with: url) { data, response, error in
        guard let data = data else {
            print("Data not found: \(error?.localizedDescription ?? "Unknown error")")
            return
        }
        if let webpageContent = String(data: data, encoding: .utf8) {
            print(webpageContent)
        }
    }
    task.resume()
}
```

सैंपल आउटपुट:
```swift
<!doctype html>
<html>
...
</html>
```

## Deep Dive (गहराई से जानिए)
वेब पेज डाउनलोडिंग का इस्तेमाल 1990 के दशक से हो रहा है जब इंटरनेट नया था। `URLSession` ऐप्लिकेशन्स को HTTP नेटवर्किंग करने की सुविधा देता है। Swift में, `URLSession` सिंपल और मल्टीपार्ट डाउनलोड्स, बैकग्राउंड डाउनलोड्स, और वेबसाइट से डाटा पाने के काम आता है। अन्य विकल्प जैसे `Alamofire` थर्ड-पार्टी लाइब्रेरी भी मौजूद हैं।

## See Also (और भी जानकारी)
- Apple's URLSession Documentation: [URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- Swift.org Documentation: [Swift.org Documentation](https://www.swift.org/documentation/)
- Networking with URLSession in Swift Article: ["Networking with URLSession"](https://www.raywenderlich.com/3244963-urlsession-tutorial-getting-started)
- Alamofire, a Swift-based HTTP networking library: [Alamofire GitHub Repository](https://github.com/Alamofire/Alamofire)
