---
title:                "डायरेक्टरी मौजूद है या नहीं जाँचना"
aliases:
- /hi/swift/checking-if-a-directory-exists.md
date:                  2024-02-03T19:09:57.599999-07:00
model:                 gpt-4-0125-preview
simple_title:         "डायरेक्टरी मौजूद है या नहीं जाँचना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
फाइल सिस्टम में एक निर्देशिका की उपस्थिति की जांच करना आपके स्विफ्ट एप्लिकेशन्स से फाइल संरचनाओं को प्रबंधन करने के लिए अनिवार्य है। यह कार्य डेवलपर्स को उनसे पढ़ने या लिखने की कोशिश करने से पहले निर्देशिकाओं की उपस्थिति की पुष्टि करने की अनुमति देता है, इस प्रकार संभावित रनटाइम त्रुटियों से बचने में मदद मिलती है।

## कैसे करें:

Swift का फाउंडेशन फ्रेमवर्क `FileManager` क्लास प्रदान करता है, जिसमें फ़ाइल सिस्टम को प्रबंधित करने के लिए विधियाँ हैं। आप `FileManager` का उपयोग करके जांच सकते हैं कि कोई निर्देशिका मौजूद है या नहीं। यहाँ यह कैसे करें इस पर एक स्निपेट है:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("निर्देशिका मौजूद है")
} else {
    print("निर्देशिका मौजूद नहीं है")
}
```

हालांकि, यह फाइलों और निर्देशिकाओं दोनों के लिए जांचता है। यदि आप विशेष रूप से यह सत्यापित करना चाहते हैं कि एक निर्देशिका मौजूद है, तो आपको `isDirectory` में एक बूलियन मान के लिए एक पॉइंटर पास करना होगा:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("निर्देशिका मौजूद है")
} else {
    print("निर्देशिका मौजूद नहीं है")
}
```

### थर्ड-पार्टी लाइब्रेरी का उपयोग करना

अभी के लिए, Swift में एक निर्देशिका के अस्तित्व की जांच करना आमतौर पर `FileManager` क्लास की मजबूती के कारण थर्ड-पार्टी लाइब्रेरीज को आवश्यक नहीं मानता। हालांकि, अधिक जटिल फाइल मैनीपुलेशन और जांच के लिए, जॉन संडेल द्वारा **Files** जैसी लाइब्रेरीज एक अधिक स्विफ्ट-अनुकूल API प्रदान करती है।

यहाँ आप इसे कैसे उपयोग कर सकते हैं:

पहले, Swift पैकेज मैनेजर के माध्यम से अपनी परियोजना में Files जोड़ें।

फिर, आप इस तरह से एक निर्देशिका के अस्तित्व की जांच कर सकते हैं:

```swift
import Files

do {
    _ = try Folder(path: "/path/to/your/directory")
    print("निर्देशिका मौजूद है")
} catch {
    print("निर्देशिका मौजूद नहीं है")
}
```

नोट: चूंकि थर्ड-पार्टी लाइब्रेरीज में परिवर्तन हो सकता है, हमेशा उपयोग और सर्वोत्तम प्रथाओं के लिए नवीनतम दस्तावेज़ीकरण का संदर्भ लें।
