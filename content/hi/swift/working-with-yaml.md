---
title:                "यामल के साथ काम करना"
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
YAML एक डेटा सीरियलाइजेशन फॉर्मेट है जो कॉन्फ़िगरेशन फ़ाइलों और डेटा को संग्रहीत करने में उपयोगी है। प्रोग्रामर इसका इस्तेमाल इसलिए करते हैं क्योंकि यह पढ़ने में आसान होता है और बहुत सारी प्रोग्रामिंग भाषाओं के साथ सहजता से मिल जाता है।

## How to: (कैसे करें:)
Swift में YAML के साथ काम करने के लिए हमें आमतौर पर किसी लाइब्रेरी की ज़रूरत होती है, क्योंकि स्टैंडर्ड लाइब्रेरी में YAML के लिए डायरेक्ट सपोर्ट नहीं है। इस उदाहरण में हम `Yams` लाइब्रेरी का इस्तेमाल करेंगे।

सबसे पहले, `Yams` इंस्टॉल करें:
```swift
// Swift Package Manager के जरिये
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

अब सिंपल YAML स्ट्रिंग पार्स करें:
```swift
import Yams

let yaml = """
name: John Doe
age: 30
isEmployed: true
"""

do {
    let data = try Yams.load(yaml: yaml) as? [String: Any]
    print(data?["name"] as? String ?? "") // Output: John Doe
} catch {
    print(error)
}
```

पार्स की गई डेटा स्ट्रक्चर्स को YAML स्ट्रिंग में डंप करना:

```swift
import Yams

let data: [String: Any] = [
    "name": "John Doe",
    "age": 30,
    "isEmployed": true
]

do {
    let yamlString = try Yams.dump(object: data)
    print(yamlString)
    /*
    age: 30
    isEmployed: true
    name: John Doe
    */
} catch {
    print(error)
}
```

## Deep Dive (गहन जानकारी)
YAML का मतलब है "YAML Ain't Markup Language" जिसे रिकर्सिव बैक्रोनिम कहते हैं। यह XML और JSON का एक विकल्प है और अक्सर कॉन्फ़िगरेशन और डेटा स्टोरेज में इस्तेमाल होता है। YAML के डेटा स्ट्रक्चर्स में लिस्ट्स, मैप्स, और स्कलर्स शामिल हैं।

प्रोग्रामर JSON या XML के बजाय YAML का इस्तेमाल करते हैं क्योंकि यह अधिक पढ़ने में सरल और इंडेंटेशन आधारित होता है। इसके अलावा, YAML में कमेंट्स और कॉम्प्लेक्स डेटा स्ट्रक्चर्स को रेप्रेजेंट करना आसान होता है।

## See Also (और जानिये)
- YAML स्पेसिफ़िकेशन: https://yaml.org/spec/1.2/spec.html
- `Yams` GitHub रेपोज़िटरी: https://github.com/jpsim/Yams
- Swift Package Manager का इस्तेमाल: https://swift.org/package-manager/
