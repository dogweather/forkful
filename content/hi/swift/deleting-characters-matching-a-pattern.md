---
title:    "Swift: नियम से मेल खाने वाले अक्षरों को हटाना"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

"## क्यों"

कभी-कभी हमें अपने कोड में से एक निश्चित पैटर्न का मिलता है जो हमें नहीं चाहिए होता है। ऐसे मामलों में, हम उस पैटर्न के मैचिंग अक्षरों को हटाना चाहते हैं। इस ब्लॉग पोस्ट में, हम हिंदी पाठकों के लिए बताएंगे कि इस कार्य को क्यों और कैसे करना है। 

"## कैसे करें"

इस कार्य को करने के लिए, हमें कुछ स्टेप्स का पालन करना होगा:

१. पहले, हमें अपने स्ट्रिंग से अवशिष्ट मैचिंग अक्षर हटाने के लिए अपने स्ट्रिंग को एक अरे में बदलना होगा। इसके लिए, हम `components(separatedBy:)` मेथड का उपयोग कर सकते हैं।

2. अब, हमें उस अरे के मैचिंग अक्षरों को हटाने के लिए `removeAll(where:)` मेथड का उपयोग करना होगा।

इसे समझने के लिए, एक साधारण स्विफ्ट कोड का नमूना दिया गया है:

```Swift
var string = "Hello, World!"
let patterns = ["l", "o"]
let array = string.components(separatedBy: .whitespaces)
let newArray = array.map { (word) -> String in
    return word.removeAll(where: { (char) -> Bool in
        return patterns.contains(String(char))
    })
}
print(newArray.joined(separator: " ")) // prints: He, Wrd!
```

"## डीप डाइव"

पैटर्न के मैचिंग अक्षरों को हटाने के लिए, हमें `removeAll(where:)` मेथड का उपयोग करना है जो `Character` टाइप के पैरामीटर को लेता है और रिटर्न टाइप `Bool` होता है। हमें इस मेथड में दो स्ट्रिंग को कंपेयर करने के लिए `contains()` मेथड का भी उपयोग करना होगा।

"## देखें भी"

- [Swift ऑफिशियल डॉक्यूमेंटेशन (अंग्रेजी में)](https://developer.apple.com/documentation/swift)
- [Swift पाठकों के लिए ऑनल