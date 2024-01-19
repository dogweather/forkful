---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

# Swift में Substrings निकालना: एक गाइड

## क्या और क्यों? 
Substrings निकालना या extracting substrings एक मुख्य programming task होता है, जिसमें आप एक string के भीतर से एक छोटे शब्द (substring) को खोजते हैं और उसे अलग करते हैं। Programmers सुबस्ट्रिंग्स निकालते हैं जब वे डेटा का विश्लेषण करते हैं, फार्म्स पर डेटा वालिडेट करते हैं, या text-based डाटा का मेक्स्ट्रक्शन करते हैं।

## कैसे:
```Swift
let str = "Hello, Swift Programming!"
let start = str.index(str.startIndex, offsetBy: 7)
let end = str.index(str.startIndex, offsetBy: 12)
let substring = str[start...end]
print(substring)
```
ऊपरी कोड में हमने "Swift" नामक substring निकाला। यह औरपुट देगा:
```Swift
Swift
```

## गहरी जानकारी:
1. **ऐतिहासिक प्रसंग**: Swift में उपयोग किए जाने वाले `index(of:)` मेथड की मदद से substrings का पता लगाने की कला कई सालों से चली आ रही है। यह एक कार्यक्षम गणना क्षमता और स्मृति क्षमता दोनों को बचाता है।

2. **विकल्प**: आप `range(of:)` का भी उपयोग कर सकते हैं जिसे ध्यानपूर्वक इस्तेमाल करना होगा क्योंकि यह अहसासी होता है और यदि इसे ध्यानपूर्वक ना इस्तेमाल किया जाए तो यह निष्पादन में पेनेल्टी का कारण हो सकता है। 

3. **आविष्कार**: `substring` का एक विशेष विमर्श Swift की लाइफसाइकल में खोजने के लिए शारीरिक स्ट्रिंग की आवश्यकता होती है। यह एक आंतरहिन संबंध होता है और जब तक `String` जीवित है, तब तक `Substring` भी जीवित रहता है।

## अन्य स्रोतों के लिए:
1. Apple Developer Documentation: [Swift Standard Library](https://developer.apple.com/documentation/swift/swift_standard_library)

2. StackOverflow: [Extract substring in Swift](https://stackoverflow.com/questions/24192881/how-to-extract-a-substring-in-swift)

3. Swift by Sundell: [Working with strings in Swift](https://www.swiftbysundell.com/basics/strings/)