---
title:    "Swift: एक पाठ फ़ाइल लिखना"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

टेक्स्ट फाइल लिखने में शामिल होने का कारण है क्योंकि यह आपको अपनी स्विफ्ट कोड को संग्रहीत करने में मदद करता है और बाद में उसे देखने या साझा करने के लिए उपयोगी होता है।

## कैसे करें

इस प्रकार से एक टेक्स्ट फाइल लिखने के लिए स्विफ्ट कोड का उदाहरण दिया गया है:

```Swift
// टेक्स्ट फाइल का पथ
let filePath = "myFile.txt"

// टेक्स्ट फाइल को स्ट्रिंग में लिखें
let text = "मेरा पहला टेक्स्ट फाइल!"

// टेक्स्ट फाइल बनाएं
FileManager.default.createFile(atPath: filePath, contents: text.data(using: .utf8))

// टेक्स्ट फाइल का स्थान प्राप्त करें
if let dir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first {
    let fileURL = dir.appendingPathComponent(filePath)
    
    do {
        // स्ट्रिंग को टेक्स्ट फाइल में लिखें
        try text.write(to: fileURL, atomically: false, encoding: .utf8)
        
        // टेक्स्ट फाइल से डेटा पढ़ें
        let textFromFile = try String(contentsOf: fileURL, encoding: .utf8)
        
        // संदेश के साथ डेटा प्रिंट करें
        print("टेक्स्ट फाइल में से पढ़ा हुआ डेटा: \(textFromFile)")
    } catch {
        print("त्रुटि: \(error)")
    }
}
```

इस कोड का आउटपुट होगा:

```
मेरा पहला टेक्स्ट फाइल!
```

## गहराई में जाएं

टेक्स्ट फाइल लिखने के लिए FileManager क्लास का उपयोग करते हुए, हम स्थानीय या स्थानीय नहीं बनाएंगे। स्थान के साथ इसके लिए पास होने वाले अन्‍य विकल्प हैं:

- उपयोगकर्ताओं के बीच टेक्स्ट फाइल साझा करें
- एक गैर-उपस्थित सर्वर पर टेक्स्ट फाइल लिखें और उसे लोड करें
- स्थानीय उपकरणों से मौसम की समीक्षा चलाएं