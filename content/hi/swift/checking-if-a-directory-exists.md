---
title:    "Swift: डायरेक्टरी मौजूद है की नहीं जाँच करना"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# क्यों

एक निर्देशिका मौजूद है या नहीं चेक करने के लिए स्विफ्ट प्रोग्रामिंग ब्लॉग पोस्ट लिखने का संदर्भ बहाना है क्योंकि यह उपयोगकर्ता को यह जानने में मदद कर सकता है कि वे उपयोगकर्ता के डेटा को सही स्थान से लोड करते हैं या नहीं।

## कैसे करें

यहां हम आपको स्विफ्ट में निर्देशिका मौजूद है या नहीं चेक करने के लिए लाभकारी कोड दिखाएंगे।

```Swift
let fileManager = FileManager.default
let path = "/Users/UserName/Documents"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory) {
    // यहां हमारा कोड निर्देशिका मौजूद होने पर चलेगा
    print("निर्देशिका मौजूद है।")
    if isDirectory.boolValue {
        print("इसका एक निर्देशिका है।")
    } else {
        print("यह एक फ़ाइल है।")
    }
} else {
    // यहां हमारा कोड निर्देशिका मौजूद नहीं होने पर चलेगा
    print("निर्देशिका मौजूद नहीं है।")
}
```

आउटपुट:

निर्देशिका मौजूद है।
इसका एक निर्देशिका है।

निर्देशिका मौजूद है।
यह एक फ़ाइल है।

## गहराई तक

अब हम निर्देशिका मौजूद है या नहीं चेक करने की गहराई में जबरदस्ती जाएंगे। जब हम `fileExists(atPath:isDirectory:)` फ़ंक्शन को कॉल करते हैं, तब हमें दो तरह के पैरामीटर पास करने के लिए कहा जाता है। `path` वास्तविक निर्देशिका या फ़ाइल का पथ है जिसे हम चेक करना चाहते हैं और `isDirectory` एक बूलियन वेरिएबल है जो हमें