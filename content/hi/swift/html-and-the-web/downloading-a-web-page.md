---
title:                "वेब पेज डाउनलोड करना"
aliases:
- /hi/swift/downloading-a-web-page/
date:                  2024-01-20T17:45:26.770113-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/downloading-a-web-page.md"
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
