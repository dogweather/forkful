---
title:                "Swift: विन्यस्त अभिव्यंजनों का प्रयोग करना"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

आज के डिजिटल दुनिया में, हर प्रोग्रामर को भी कुछ न भी करे, उन्हें खुद को रेगुलर एक्सप्रेशन्स का सामना जरूर करना पड़ता है। इससे कोडिंग प्रक्रिया आसान होने के साथ-साथ उनके प्रोग्राम्स का परफॉर्मेंस भी बढ़ जाता है।

## कैसे

```Swift
let regex = try! NSRegularExpression(pattern: "a|e|i|o|u")
let input = "Hello, Swift!"
let matches = regex.matches(in: input, range: NSRange(input.startIndex..., in: input))
for match in matches {
    let matchRange = match.range
    if let range = Range(matchRange, in: input) {
        print(input[range])
    }
}
```

यह कोड हमें "Hello, Swift!" रास्ता में से गए वर्णों को दिखाएगा। हम चाहें तो उनको अन्य वर्णों से भी बदल सकते हैं।

## गहराई से जानकारी

रेगुलर एक्सप्रेशन्स एक पावरफुल और विस्तृत उपकरण हो सकते हैं। यदि आपको कोई भी डिजाइन डेटो या पैटर्न ढूंढना हो तो इससे सहायता ले सकते हैं। इसके अलावा, आप सीख सकते हैं कि किस तरह से अलग-अलग मेटाचिंग और ग्रूपिंग विकल्प काम करते हैं। आप स्ट्रिंग में परिवर्तन करने के लिए भी इसका उपयोग कर सकते हैं।

## देखें भी

- [NSRegularExpression कक्षा डॉक्यूमेंटेशन](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Swift कोर्स - रेगुलर एक्सप्रेशन्स से काम करना सिखे](https://www.udemy.com/course/swift-regular-expressions/)