---
title:                "Swift: नियमित अभिव्यक्तियों का उपयोग"
simple_title:         "नियमित अभिव्यक्तियों का उपयोग"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

**## क्यों**

रेगुलर एक्सप्रेशन का प्रयोग Swift में किसी भी पाठ या डेटा का संशोधन करने के लिए किया जाता है। यह एक शक्तिशाली, सुजाव दिया गया उपकरण है जो लंबित पाठ और शब्दों को खोजने और संशोधित करने में मदद करता है।

**## कैसे करें**

रेगुलर एक्सप्रेशन को Swift में उपयोग करने के लिए, हम `NSRegularExpression` और `NSPredicate` क्लास का उपयोग कर सकते हैं। नीचे दिए गए उदाहरण में, हम एक पाठ से सटीक टाइमस्टाम्प को खोजने के लिए एक रेगुलर एक्सप्रेशन का उपयोग करते हैं। फिर, हम इसे एक स्ट्रिंग में परिवर्तित करते हैं और उसका उपयोग करके उपयुक्त स्क्रीन के फार्मेट में सटीक तारीख को प्रदर्शित करते हैं।

```Swift
let regex = try! NSRegularExpression(pattern: "\\d{10}")
let string = "Created at 1565586482"
let timestamp = Int(regex.stringByReplacingMatches(in: string, options: [], range: NSMakeRange(0, string.count), withTemplate: ""))
let time = Date(timeIntervalSince1970: TimeInterval(timestamp!))
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd MMM yyyy"
let formattedDate = dateFormatter.string(from: time)
print(formattedDate)

// Output: 12 Aug 2019
```

**## गहराई में जाएं**

इस उदाहरण में, हमने एक रेगुलर एक्सप्रेशन का उपयोग करके बहुत ही उपयोगी तारीख कार्यों के विषय में सीखा है। रेगुलर एक्सप्रेशन सीखना थोड़ा मुश्किल हो सकता है, लेकिन यह लायक है। आप इसके माध्यम से एक बार ज्ञात करने के बाद अधिक से अधिक समय बचा सकते हैं और उपयोगी कार्यों को संपादित और स्थायी बना सकते हैं।

**## देखें भी**

- [Swift Regular Expressions Tutorial](https://www.raywenderlich.com/3016-regular-expressions-tutorial-getting-started)
- [NSRegularExpression Documentation