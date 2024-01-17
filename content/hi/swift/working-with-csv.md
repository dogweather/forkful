---
title:                "CSV के साथ काम करना"
html_title:           "Swift: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## CSV क्या है और क्यों करते हैं?
CSV (Comma Separated Values) एक फॉर्मेट है जो डेटा को अलग-अलग कॉलमों में अलग करता है ताकि उसे आसानी से स्प्रेडशीट या डेटा बेस हो या अन्य प्रोग्रामों में इम्पोर्ट/एक्सपोर्ट किया जा सके। प्रोग्रामर इस्तेमाल करते हैं CSV ताकि वे अपने प्रोग्राम को और उपयोगकर्ता अन्य स्थानों से डेटा लाने या स्टोर करने में सहायता कर सकें।

## कैसे:
```Swift
import Foundation

// एक CSV फाइल से डेटा पढ़ें
let csvFilePath = "data.csv"
do {
    let csvData = try String(contentsOf: URL(fileURLWithPath: csvFilePath))
    let rows = csvData.components(separatedBy: "\n")
    
    // सभी पंक्तियों को लूप करें
    for row in rows {
        // प्रत्येक पंक्ति में अलग अलग कॉलमों को अलग करें
        let columns = row.components(separatedBy: ",")
        
        // प्रत्येक कॉलम को प्रिंट करें
        for column in columns {
            print(column)
        }
    }
} catch {
    print("फाइल खोलने में गड़बड़ी हुई।")
}

// CSV फाइल में नए डेटा लिखें
let data = "1, 2, 3\n4, 5, 6\n7, 8, 9"
do {
    try data.write(toFile: "new_data.csv", atomically: true, encoding: .utf8)
    print("नई CSV फाइल बनाई गई।")
} catch {
    print("फाइल लिखने में गड़बड़ी हुई।")
}
```

**आउटपुट:**
```
1
2
3
4
5
6
7
8
9

नई CSV फाइल बनाई गई।
```

## डीप डाइव:
- CSV को वर्कशीट फाइलों में प्रयोग करने की शुरुआत 1972 में अमेरिकी कंपनी IBM ने की थी।
- अल्टर्नेटिव के रूप में, प्रोग्रामर अक्सर डेटा को फाइल्स से पढ़ना और लिखना चुनते हैं लेकिन CSV के फायदे वहाँ हो सकते हैं जहाँ कि डेटा को अलग अलग तरीके से स्टोर किया जाए जैसे कि स्प्रेडशीट फाइल, डेटाबेस या अन्य प्रोग्रामों में।
- CSV का प्रयोग किसी भी भाषा में किया जा सकता है, इसलिए प्रोग्रामरों को इसकी ज्ञानकारी होना आवश्यक है।

## देखें भी:
- [Apple डॉक्यूमेंटेशन उदाहरण](https://developer.apple.com/documentation/foundation/nsstring/1409103-components)
- [CSV फाइल की संरचना के बारे में अधिक जानकारी के लिए](https://www.computerhope.com/issues/ch001356.htm)