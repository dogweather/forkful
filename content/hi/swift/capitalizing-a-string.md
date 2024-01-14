---
title:    "Swift: स्ट्रिंग को कैपिटलाइज करना"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## क्यों

कोई चीज़ संभव है इसलिए आपने अपने कोड में एक स्ट्रिंग को ट्रांसफॉर्म करना चाहते हैं, उदाहरण के लिए, इसे पूरे के रूप में बड़े अक्षरों में प्रदर्शित करना। यह एक आसान और संभवतः स्वतंत्र चीज़ हो सकती है जो आपके कोड को और स्वचालित बना सकती है।

## कैसे करें

आपको स्ट्रिंग को कैपिटलाइज करने के लिए एक बिल्ट-इन फ़ंक्शन का उपयोग करना होगा, जिसमें आप स्ट्रिंग को देने के लिए हमेशा ही कैपिटलिज़ कैसे पाएं।

```Swift
let myString = "hello world"
let capitalizedString = myString.capitalized
print(capitalizedString) // Prints "Hello World"
```

जैसा कि आप देख सकते हैं, हमने `capitalized` फ़ंक्शन का अपना स्ट्रिंग के साथ उपयोग किया और इसे प्रिंट किया है, जिससे हमारा स्ट्रिंग उम्दा ढंग से बड़ा लिखा होता है।

## डीप डाइव

यदि आप अपनी दृष्टिकोण को विस्तार देना चाहते हैं, तो आप `capitalized` फ़ंक्शन के पीछे क्या हो रहा है इसकी गहराई में जानने के लिए इसे इंस्पेक्ट कर सकते हैं। आप यहां यह देख सकते हैं कि यह एक प्रक्रिया को चलाता है जो इन चरित्रों को बड़े अक्षरों में परिवर्तित करता है और फिर इसे एक नए स्ट्रिंग में रखता है।

## देखें भी

- [Apple Docs](https://developer.apple.com/documentation/foundation/nsstring/1414084-capitalized)
- [Hacking with Swift](https://www.hackingwithswift.com/articles/162/one-simple-trick-to-transform-a-string)
- [Stack Overflow](https://stackoverflow.com/questions/26396540/how-to-capitalize-first-letter-of-each-word-like-a-2-word-city-name)