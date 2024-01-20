---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Swift में अस्थायी फ़ाइल बनाना

## क्या और क्यों?

अस्थायी फ़ाइलें वे फ़ाइलें होती हैं जिनका उपयोग केवल अस्थायी समय के लिए किया जाता है। प्रोग्रामर्स अस्थायी फ़ाइलें तब उत्पन्न करते हैं जब उन्हें कड़ी, तथापि अस्थाई, डेटा संग्रहण की आवश्यकता होती है।

## कैसे:

रद्दीकरण के लिए `FileManager` का उपयोग करना यहाँ एक उदाहरण है:

```Swift
import Foundation

let tempDirectoryURL = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
let targetURL = tempDirectoryURL.appendingPathComponent(UUID().uuidString)

do {
    try "Temporary file content".write(to: targetURL, atomically: true, encoding: String.Encoding.utf8)
} catch {
    print("Error creating a temporary file")
}
```
## गहरी डाइव:

हिस्टोरिकल कंटेक्स्ट: अस्थायी फ़ाइलों का उपयोग बहुत पुराने समय से किया जा रहा है, लेकिन Swift ने इसे अधिक सुरक्षित और आसान बना दिया है।

विकल्प: डेटा के लिए डाटाबेस या इन-मेमोरी डाटा संग्रहण स्थानों का उपयोग किया जा सकता है, जैसे Swift के `Array` या `Dictionary` ऑब्जेक्ट।

कार्यान्वयन विवरण: `UUID().uuidString` का उपयोग रेंडम, अद्वितीय फ़ाइल नाम उत्पन्न करने के लिए किया जाता है। `NSTemporaryDirectory()` फंक्शन अस्थायी डायरेक्टरी का पथ लौटाता है।

## भी देखें:

- [Apple प्रमाण पत्रों: FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift पत्राचार: URL](https://developer.apple.com/documentation/foundation/url)
- [Apple प्रमाण-पत्रों: UUID](https://developer.apple.com/documentation/foundation/uuid)
- [Swift पत्राचार: String](https://developer.apple.com/documentation/swift/string)