---
title:    "Swift: स्ट्रिंग को छोटे अक्षर में रूपांतरित करना"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी व्यक्ति स्ट्रिंग को लोअर केस में कनवर्ट करने में क्यों आगे बढ़ना चाहेगा, उसका करण है कि लोअर केस में टेक्स्ट पढ़ना और लिखना आसान होता है और उसे डेटा की संरचना करने के लिए उपयोगी बनाता है।

## कैसे करें

```Swift
let str = "HELLO WORLD"
let lowerCaseStr = str.lowercased()
print(lowerCaseStr)
```

आप पहले स्ट्रिंग को `lowercased()` फ़ंक्शन के माध्यम से लोअर केस में कनवर्ट कर सकते हैं और फिर स्क्रीन पर प्रिंट कर सकते हैं। इससे आपको आउटपुट के रूप में "hello world" मिलेगा।

## डीप डाइव

लोअर केस में स्ट्रिंग कनवर्ट करना केवल तब उपयोगी नहीं होता है, बल्कि यह स्ट्रिंग के भिन्न अक्षरों को भी अलग करने में मदद करता है। जैसे कि हमारे पास "Hello" और "hello" दो स्ट्रिंग हैं तो कंप्यूटर उन्हें अलग स्ट्रिंग्स के रूप में समझेगा क्योंकि एक स्ट्रिंग का केस सेंसिटिव होता है।

## देखे भी

अधिक सूत्रिर्लेख (see also) के लिए निचे दिए गए लिंक जाँचें।

- [String.lowercased() Documentation](https://developer.apple.com/documentation/swift/string/3128489-lowercased)
- [Swift String Manipulation Tutorial](https://www.tutorialspoint.com/swift/swift_string_manipulation.htm)
- [An Introduction to Swift Strings](https://www.appcoda.com/swift-string/)