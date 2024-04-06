---
date: 2024-01-20 17:45:26.770113-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u0948\
  \u0902\u092A\u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T21:53:54.870122-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u0948\u0902\u092A\
  \u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
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
