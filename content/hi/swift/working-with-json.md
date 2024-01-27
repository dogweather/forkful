---
title:                "JSON के साथ काम करना"
date:                  2024-01-19
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-json.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON (JavaScript Object Notation) डेटा को स्टोर और ट्रांसपोर्ट करने का एक तरीका है। प्रोग्रामर इसका इस्तेमाल डेटा को आसानी से पढ़ने और एपीआईस के साथ संवाद स्थापित करने के लिए करते हैं।

## कैसे करें:

```Swift
import Foundation

// एक JSON स्ट्रिंग
let jsonString = """
{
    "name": "रोहन",
    "age": 30,
    "isDeveloper": true
}
"""

// JSON को Swift डिक्शनरी में पार्स करना
if let jsonData = jsonString.data(using: .utf8) {
    do {
        let dictionary = try JSONSerialization.jsonObject(with: jsonData, options: []) as? [String: Any]
        print(dictionary?["name"] ?? "नाम नहीं मिला")
    } catch {
        print("JSON पार्सिंग में एरर: \(error)")
    }
}
```

सैंपल आउटपुट:
```
रोहन
```

## गहराई में जानकारी:

JSON सरल और लाइटवेट है, जो इसे RESTful APIs के लिए पॉप्युलर चॉइस बनाता है। इतिहास में, XML जैसे विकल्प थे लेकिन JSON ने इसकी सरलता के कारण बाज़ी मार ली। Swift में, `JSONSerialization` क्लास के जरिए या `Codable` प्रोटोकोल के इस्तेमाल से JSON का हैंडलिंग किया जाता है।

## सी ऑल्सो:

- स्विफ्ट डॉक्युमेंटेशन - [JSONSerialization](https://developer.apple.com/documentation/foundation/jsonserialization)
