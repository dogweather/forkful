---
title:                "Swift: yaml के साथ काम करना"
simple_title:         "yaml के साथ काम करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

यामल के साथ काम करने की वजह सिर्फ कुछ ही शब्दों में समझाया जा सकता है। यह एक आसान और लोगों के बीच लोकप्रिय भाषा है जो स्विफ्ट प्रोग्रामिंग में वास्तविक जगह बनाती है।

## कैसे करें

यदि आप स्विफ्ट प्रोग्रामिंग के अभ्यासक या शुरुआती विकासक हैं, तो YAML को सीखना आसान हो सकता है। आप कॉड की सुरक्षा तथा दुगन्धिमा में सुधार करने और प्रमाणित करने के लिए YAML का उपयोग कर सकते हैं। निम्नलिखित स्निपेट में, हम अपने स्विफ्ट कोड में YAML फ़ाइल बनाना और पार्स करना सीखेंगे:

```Swift
let personalInfo = [
    "name": "अमित",
    "age": 25,
    "city": "दिल्ली"
]

let yaml = YAML(personalInfo)
print(yaml) // उत्पादन: उत्पादन: "नाम": "अमित", "आयु": 25, "शहर": "दिल्ली"
```

## गहराई में जानकारी

जब आप YAML संरचना पर गहराई से समझने लगते हैं, तो इसमें आपके अधिकार और फायदे हो सकते हैं। YAML में अधिक जानकारी प्राप्त करने के लिए, आप YAML दस्तावेज़ीकरण के साथ साथ GitHub और Stack Overflow के समाधान देख सकते हैं।

## इसके अलावा देखें

और अधिक स्विफ्ट प्रोग्रामिंग के बारे में जानें:

- [Swift मूल भाग 1: शुरुआत करना](https://www.iosappblogs.com/2018/02/03/swift-beginners-guide/)
- [Swift मूल भाग 2: हमलोगों को अपने Truth के साथ सही ढंग से तोलना सीखें](https://swiftrocks.com/initial-commit-1dd849192bf4)
- [Swift में कैसे Lists हासिल करें](https://www.hackingwithswift.com/quick-start/understanding-swift/why-does-swift-have-arrays-and-sets#:~:text=Swift%20uses%20arrays%20for%20