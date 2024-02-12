---
title:                "JSON के साथ काम करना"
date:                  2024-02-03T19:24:50.248127-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Swift में JSON के साथ काम करना डेटा इंटरचेंज के लिए एक हल्के डेटा प्रारूप से निपटने का मतलब है। प्रोग्रामर्स JSON का उपयोग एक सर्वर और वेब एप्लिकेशन के बीच डेटा प्रेषित करने के लिए करते हैं क्योंकि यह पठनीय और मानव और मशीनों के लिए पार्स करना आसान है।

## कैसे करें:

Swift JSON पार्सिंग को `Codable` प्रोटोकॉल के साथ सीधा बनाता है। यहाँ जानिए कि JSON को Swift ऑब्जेक्ट में कैसे डिकोड करें:

```Swift
import Foundation

// एक मॉडल परिभाषित करें जो Codable के अनुसार हो
struct User: Codable {
    var name: String
    var age: Int
}

// JSON स्ट्रिंग
let jsonString = """
{
    "name": "John Doe",
    "age": 30
}
"""

// JSON स्ट्रिंग को Data में परिवर्तित करें
if let jsonData = jsonString.data(using: .utf8) {
    // Decode JSON डेटा को User ऑब्जेक्ट में
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("नाम: \(user.name), आयु: \(user.age)")
    } catch {
        print("JSON डिकोडिंग में त्रुटि: \(error)")
    }
}
```

नमूना आउटपुट:
```
नाम: John Doe, आयु: 30
```

## गहराई में जाने पर

JSON (JavaScript Object Notation) को डगलस क्रॉकफोर्ड द्वारा इसे निर्दिष्ट करने के बाद से 2000 के दशक की शुरुआत से व्यापक रूप से अपनाया गया है। इसके सरल सिंटैक्स और बेहतर प्रदर्शन के कारण यह कई उपयोग केसेस के लिए XML की जगह ले लिया है। जबकि Swift का `Codable` JSON के लिए जाना-माना है, गैर-Codable-अनुरूप प्रकारों से निपटने पर `JSONSerialization` जैसे विकल्प मौजूद हैं। अंदरूनी तौर पर, `Codable` निचले स्तर की पार्सिंग को छुपाता है और सीरियलाइजेशन/डिसीरीयलाइजेशन को

## देखें भी

- आधिकारिक Swift ब्लॉग में JSON और Swift पर अधिक खोजें: [Swift.org](https://swift.org/blog/)
- `Codable` दस्तावेज़ीकरण देखें: [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- जटिल JSON संरचनाओं के लिए, [GitHub](https://github.com/SwiftyJSON/SwiftyJSON) पर उपलब्ध SwiftyJSON जैसे थर्ड-पार्टी पुस्तकालयों पर विचार करें।
