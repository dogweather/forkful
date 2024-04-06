---
date: 2024-01-20 17:45:26.770113-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0935\u0947\
  \u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\u0921\u093F\u0902\
  \u0917 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 1990 \u0915\
  \u0947 \u0926\u0936\u0915 \u0938\u0947 \u0939\u094B \u0930\u0939\u093E \u0939\u0948\
  \ \u091C\u092C \u0907\u0902\u091F\u0930\u0928\u0947\u091F \u0928\u092F\u093E \u0925\
  \u093E\u0964 `URLSession` \u0910\u092A\u094D\u0932\u093F\u0915\u0947\u0936\u0928\
  \u094D\u0938 \u0915\u094B HTTP \u0928\u0947\u091F\u0935\u0930\u094D\u0915\u093F\u0902\
  \u0917 \u0915\u0930\u0928\u0947 \u0915\u0940 \u0938\u0941\u0935\u093F\u0927\u093E\
  \u2026"
lastmod: '2024-04-05T22:51:07.581710-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0935\u0947\u092C\
  \ \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\u0921\u093F\u0902\u0917\
  \ \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 1990 \u0915\u0947\
  \ \u0926\u0936\u0915 \u0938\u0947 \u0939\u094B \u0930\u0939\u093E \u0939\u0948 \u091C\
  \u092C \u0907\u0902\u091F\u0930\u0928\u0947\u091F \u0928\u092F\u093E \u0925\u093E\
  \u0964 `URLSession` \u0910\u092A\u094D\u0932\u093F\u0915\u0947\u0936\u0928\u094D\
  \u0938 \u0915\u094B HTTP \u0928\u0947\u091F\u0935\u0930\u094D\u0915\u093F\u0902\u0917\
  \ \u0915\u0930\u0928\u0947 \u0915\u0940 \u0938\u0941\u0935\u093F\u0927\u093E \u0926\
  \u0947\u0924\u093E \u0939\u0948\u0964 Swift \u092E\u0947\u0902, `URLSession` \u0938\
  \u093F\u0902\u092A\u0932 \u0914\u0930 \u092E\u0932\u094D\u091F\u0940\u092A\u093E\
  \u0930\u094D\u091F \u0921\u093E\u0909\u0928\u0932\u094B\u0921\u094D\u0938, \u092C\
  \u0948\u0915\u0917\u094D\u0930\u093E\u0909\u0902\u0921 \u0921\u093E\u0909\u0928\u0932\
  \u094B\u0921\u094D\u0938, \u0914\u0930 \u0935\u0947\u092C\u0938\u093E\u0907\u091F\
  \ \u0938\u0947 \u0921\u093E\u091F\u093E \u092A\u093E\u0928\u0947 \u0915\u0947 \u0915\
  \u093E\u092E \u0906\u0924\u093E \u0939\u0948\u0964 \u0905\u0928\u094D\u092F \u0935\
  \u093F\u0915\u0932\u094D\u092A \u091C\u0948\u0938\u0947 `Alamofire` \u0925\u0930\
  \u094D\u0921-\u092A\u093E\u0930\u094D\u091F\u0940 \u0932\u093E\u0907\u092C\u094D\
  \u0930\u0947\u0930\u0940 \u092D\u0940 \u092E\u094C\u091C\u0942\u0926 \u0939\u0948\
  \u0902\u0964."
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

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
