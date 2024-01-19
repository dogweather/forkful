---
title:                "डायरेक्टरी मौजूद है या नहीं जांचना"
html_title:           "TypeScript: डायरेक्टरी मौजूद है या नहीं जांचना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं जांचना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Swift में डायरेक्टरी मौजूद है या नहीं का पता लगाना  

## क्या और क्यों?

**Swift** में, एक डायरेक्टरी मौजूद है या नहीं, इस की जाँच हम 'Checking if a directory exists' कहते हैं। यह जांच तभी की जाती है जब हमें सुनिश्चित करना हो कि हमारे पास वास्तविक में उस फ़ाइल या फ़ोल्डर का उपयोग करने के लिए पहुँच है जिस पर हम काम करना चाहते हैं।

## कैसे करें:

निम्नलिखित कोड स्निपेट एक डायरेक्टरी मौजूद होने की जांच करता है:

```Swift
import Foundation

let fileManager = FileManager.default
let folderPath = "/path/to/directory"

if fileManager.fileExists(atPath: folderPath) {
    print("डाइरेक्टरी मौजूद है")
} else {
    print("डाइरेक्टरी मौजूद नहीं है")
}
```

यदि डायरेक्टरी मौजूद है, तो यह "डाइरेक्टरी मौजूद है" प्रिंट करेगा। अन्यथा, "डाइरेक्टरी मौजूद नहीं है" प्रिंट होगा।

## गहराई में :

Swift की 'FileManager' क्लास का उपयोग करके हम डायरेक्टरी मौजूद है या नहीं, इसकी जांच कर सकते हैं। 'fileExists(atPath:)' एक बूलियन वापस करता है, जो डायरेक्टरी की मौजूदगी का पता लगाता है।

Swift के पुराने versions में यह function नहीं था, और developers को अधिक complex और verbose कोड लिखना पड़ता था।

इसके विकल्प रूप में, आप `isReadableFile(atPath:)` function का भी उपयोग कर सकते हैं, जो निर्दिष्ट पथ पर एक फ़ाइल की पठनीयता की जांच करता है।

## और देखें:

अधिक जानकारी और Swift के विभिन्न तरीकों का उपयोग करके डायरेक्टरी की जांच करने के लिए, इन लिंक्स पर जाएँ:

1. [Apple Developer Documentation - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
2. [StackOverflow - How to check if a directory exists in Swift?](https://stackoverflow.com/questions/24097826/read-and-write-data-from-text-file)

आपको कोडिंग में अगली बारी में मिलेंगे, मज़े करें और संगठित रहें!