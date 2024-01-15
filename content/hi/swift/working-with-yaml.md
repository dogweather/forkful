---
title:                "yaml पर काम करना"
html_title:           "Swift: yaml पर काम करना"
simple_title:         "yaml पर काम करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

यैमल की संरचना सुगमता और स्पष्टता के कारण स्विफ्ट आभासशाली प्रोग्रामिंग भाषा के लिए अच्छा विकल्प है। इसके माध्यम से कोड को प्रारूपित करना आसान होता है और डेटा हैंडलिंग को भी सुविधाजनक बनाता है।

## कैसे करें

```Swift
import YAML

// डॉक्यूमेंट लोड करें
let yaml = YAML.load("""
name: John
age: 30
address:
  street: Main Street
  city: New York
""")

// डॉक्यूमेंट में से डेटा एक्सेस करें
let name = yaml["name"].string
let age = yaml["age"].int
let street = yaml["address"]["street"].string
let city = yaml["address"]["city"].string

// नए डॉक्यूमेंट बनाएं
let newDoc = YAML.dictionary(["fruit": "apple", "color": "red"])

print(newDoc)
// {"fruit": "apple", "color": "red"}
```

## गहराई में जाएं

यैमल फॉर्मैट में डेटा टाइप को रिप्रेजेंट करने के लिए विभिन्न टॉपलेवल टाइप जैसे स्ट्रिंग, इंटीजर, बूलियन आदि का उपयोग किया जा सकता है। साथ ही कस्टम डेटा टाइप को भी समर्थन किया जाता है, जो अनुकूलित एप्लिकेशन बनाने में मददगार होता है।

## देखें भी

- [YAML डॉक्यूमेंटेशन](https://yaml.org/)
- [Yams प्रोजेक्ट संग्रहालय](https://github.com/jpsim/Yams)
- [YAML में स्ट्रिंग और नंबर एक्सेस करने के लिए फंक्शन](https://stackoverflow.com/questions/49481988/how-to-access-string-and-number-in-yaml)