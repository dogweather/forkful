---
title:                "Swift: स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों
यदि आप Swift में प्रोग्रामिंग करते हैं तो आपने शायद capitalizing या वक्रीकरण के बारे में सुना होगा। अगर आप आज भी इसके बारे में जानना चाहते हैं, तो यह लेख आपके लिए ही है।

## कैसे करें
इसके लिए, हमें एक string को capitalize करने के लिए String वर्ग के एक विशेष तरीके या तकनीक का उपयोग करना होगा। इसके लिए हम ```Swift.uppercased()``` का उपयोग कर सकते हैं। इसका उपयोग निम्न तरह से किया जा सकता है:

```Swift
let str = "hello"
print(str.uppercased())
// Output: "HELLO"
```

पहले, हम उपरोक्त कोड के माध्यम से "str" नाम का एक string बनाते हैं। उसके बाद, हम ```uppercased()``` तकनीक का उपयोग करके इस string को capitalized करते हैं और अंत में हम उसको प्रिंट करते हैं।

## गहराई में
Capitalizing string के बारे में और गहराई में जानने के लिए, आप हमारे द्वारा लिखित एक अन्य आलेख पढ़ सकते हैं जो आपको Swift में strings के साथ काम करने के बारे में विस्तृत जानकारी देगा।

## देखें भी
- [Strings Tutorial in Swift](https://www.raywenderlich.com/147162/swift-tutorial-part-1-expressions-variables-constants)
- [Working with Strings in Swift](https://medium.com/@surabhisatamkari/working-with-strings-in-swift-431c6f788949)
- [Capitalizing and Lowercasing Strings in Swift](https://swiftrocks.com/capitalizing-and-lowercasing-strings-in-swift.html)