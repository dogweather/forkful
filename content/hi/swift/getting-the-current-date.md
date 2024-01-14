---
title:    "Swift: तारीख को प्राप्त करना"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप जानते हैं कि स्विफ्ट में वर्तमान तिथि प्राप्त करने का लोग को यहाँ क्यों होना चाहिए? सुबह जगने के लिए तारीका हो सकता है या किसी अन्य कारण की वजह से आपको जान की जरूरत हो सकती है।

## कैसे करें

आईएसओ 8601 प्रारूप में वर्तमान तिथि बोला साल, महीना और दिन कोड है कि आप आसानी से वर्तमान तिथि हासिल कर सकते हैं। यह कार्य है कि स्विफ्ट में अपने कोड में होने के लिए रात को स्विच गिरना जरूरी है।

```Swift
let date = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let dateString = dateFormatter.string(from: date)
print(dateString)
```
परिणाम:
```
2021-09-23
```

## गहरी जांच

वर्तमान तिथि को स्विफ्ट में हासिल करना एक आसान कार्य है, लेकिन समझने के लिए थोड़ा समय मुहाया कर सकते हैं। आप स्विफ्ट की इस फीचर के बारे में और अधिक जान सकते हैं स्विफ्ट के [आधिकारिक डॉक्यूमेंटेशन](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html) संबंधित विषयों पर।

## देखें भी

- [स्विफ्ट प्रोग्रामिंग को शुरू करें](https://phoenixnap.com/kb/swift-tutorial-for-beginners)
- [स्विफ्ट में तारीखों को संचालित करना](https://www.swiftbysundell.com/basics/working-with-dates/)
- [स्विफ्ट में तारीख एकीकरण](https://medium.com/swift2go/date-display-and-formatting-in-swift-5-ad6a4aeff0b2)