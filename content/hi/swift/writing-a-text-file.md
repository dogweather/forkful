---
title:                "एक टेक्स्ट फ़ाइल लिखना"
aliases:
- hi/swift/writing-a-text-file.md
date:                  2024-02-03T19:30:47.819638-07:00
model:                 gpt-4-0125-preview
simple_title:         "एक टेक्स्ट फ़ाइल लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Swift में टेक्स्ट फ़ाइल लिखने से आपको फाइलसिस्टम पर स्ट्रिंग डेटा को स्थायी रूप से संग्रहीत करने की अनुमति मिलती है, जो कॉन्फिगरेशन सेटिंग्स, उपयोगकर्ता डेटा, या लॉग्स को सहेजने जैसे कार्यों के लिए आवश्यक है। प्रोग्रामर्स अक्सर इसे ऐप लॉन्च के बीच डेटा को बनाए रखने, अलग-अलग भागों के एप्लीकेशन के बीच डेटा को साझा करने, या डेटा को अन्य प्रोग्रामों द्वारा उपयोग के लिए निर्यात करने के लिए करते हैं।

## कैसे करें:

### स्विफ्ट स्टैंडर्ड लाइब्रेरी का उपयोग करते हुए

स्विफ्ट की स्टैंडर्ड लाइब्रेरी में टेक्स्ट फाइलें लिखने के लिए आवश्यक सभी उपकरण शामिल हैं। यहाँ एक मूल दृष्टिकोण है:

```swift
import Foundation

let content = "Hello, Wired पाठकों! Swift सीखना मजेदार है।"
let filePath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let fileName = "\(filePath)/example.txt"

do {
    try content.write(toFile: fileName, atomically: false, encoding: String.Encoding.utf8)
    print("फ़ाइल सफलतापूर्वक लिखी गई")
} catch let error as NSError {
    print("URL पर लिखने में विफल: \(fileName), त्रुटि: " + error.localizedDescription)
}
```

यह कोड स्निपेट `example.txt` नामक फाइल में एक स्ट्रिंग को दस्तावेज़ निर्देशिका में लिखता है। यह स्विफ्ट के डू-ट्राई-कैच त्रुटि हैंडलिंग का उपयोग करके संभावित त्रुटियों को संभालता है।

### अधिक नियंत्रण के लिए FileManager का उपयोग

फाइल विशेषताओं पर अधिक नियंत्रण या यह जांचने के लिए कि फाइल पहले से मौजूद है या नहीं, `FileManager` का उपयो�

```swift
import Foundation

let fileManager = FileManager.default
let directories = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
if let documentDirectory = directories.first {
    let fileURL = documentDirectory.appendingPathComponent("example.txt")
    let content = "स्विफ्ट के लिए फाइल प्रबंधन की खोज प्रबुद्ध करने वाली है।"

    if fileManager.fileExists(atPath: fileURL.path) {
        print("फाइल पहले से मौजूद है")
    } else {
        do {
            try content.write(to: fileURL, atomically: true, encoding: .utf8)
            print("फ़ाइल सफलतापूर्वक बनाई और लिखी गई")
        } catch {
            print("फाइल लिखने में त्रुटि: \(error)")
        }
    }
}
```

### तृतीय-पक्ष लाइब्रेरियों का उपयोग

Swift में फाइलसिस्टम ऑपरेशन्स के लिए एक लोकप्रिय तृृतीय-पक्ष लाइब्रेरी है `Files` by John Sundell:

सबसे पहले, अपने परियोजना में Files को जोड़ें, आमतौर पर Swift Package Manager के माध्यम से।

```swift
// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "आपकाPackageअंक",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "आपकाTargetअंक",
            dependencies: ["Files"]),
    ]
)
```

फिर, इसका उपयोग फाइल में लिखने के लिए करें:

```swift
import Files

do {
    let file = try File(path: "/path/to/your/directory/example.txt")
    try file.write(string: "Swift और Files लाइब्रेरी एक शक्तिशाली संयोजन बनाते हैं।")
    print("Files लाइब्रेरी का उपयोग करके फाइल सफलतापूर्वक लिखी गई।")
} catch {
    print("एक त्रुटि हुई: \(error)")
}
```

`Files` लाइब्रेरी के साथ, फाइलों को संभालना अधिक सरल हो जाता है, जिससे आप अपने एप्लीकेशन के व्यावसायिक तर्क पर ध्यान केंद्रित कर सकते हैं बजाय फाइल प्रबंधन की सूक्ष्मताओं के।
