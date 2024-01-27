---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV (Comma-separated values) से हम सरलता से डेटा स्टोर और एक्सचेंज कर सकते हैं. प्रोग्रामर्स इसे इसलिए इस्तेमाल करते हैं क्योंकि यह फॉर्मेट बहुत सारे एप्लीकेशन्स के साथ कॉम्पेटिबल है और डेटा प्रॉसेसिंग में आसानी प्रदान करता है.

## How to: (कैसे करें:)
```Swift
import Foundation

// CSV फाइल की कंटेंट्स को पढ़ना
let csvString = """
name,age,city
John Doe,29,New York
Jane Smith,35,Los Angeles
"""

// CSV डेटा को पार्स करने का फंक्शन
func parseCSV(contents: String) {
    let rows = contents.components(separatedBy: "\n")
    
    for row in rows {
        let columns = row.components(separatedBy: ",")
        print("Name: \(columns[0]), Age: \(columns[1]), City: \(columns[2])")
    }
}

// पार्स फंक्शन को कॉल करना
parseCSV(contents: csvString)
```
Sample Output:
```
Name: name, Age: age, City: city
Name: John Doe, Age: 29, City: New York
Name: Jane Smith, Age: 35, City: Los Angeles
```

## Deep Dive (गहराई से जानकारी):
CSV 1972 से डेटा के सिम्पल एक्सचेंज फॉर्मेट के रूप में इस्तेमाल हो रहा है. इसके विकल्प में जेसन (JSON), एक्सएमएल (XML), और एक्सेल फॉर्मेट्स (XLS) आते हैं. हालांकि, CSV की सादगी और सार्वत्रिकता इसे हमेशा की तरह पसंदीदा बनाए रखती है.

## See Also (यह भी देखें):
- CSV के बारे में और [Apple के Swift डॉक्यूमेंटेशन](https://developer.apple.com/documentation) में पढ़ें.
- Swift में डेटा प्रॉसेसिंग के लिए [Swift Algorithms GitHub रिपोजिटरी](https://github.com/apple/swift-algorithms).
- प्रोग्रामेटिक CSV प्रॉसेसिंग के लिए [SwiftCSV GitHub रिपोजिटरी](https://github.com/swiftcsv/SwiftCSV).
