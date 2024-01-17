---
title:                "टेक्स्ट खोजना और बदलना"
html_title:           "Swift: टेक्स्ट खोजना और बदलना"
simple_title:         "टेक्स्ट खोजना और बदलना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट को खोज और बदलना हर प्रोग्रामर के लिए एक जरूरी काम है। यह उन्हें कोड की सुधार करने और बग्स को ठीक करने में मदद करता है।

## कैसे:

```Swift
let str = "Hello, World!"
let newStr = str.replacingOccurrences(of: "World", with: "Swift")
print(newStr)

// Output: Hello, Swift!
```

इस उदाहरण में, हमने ```replacingOccurrences``` फ़ंक्शन का उपयोग करके वर्तमान स्ट्रिंग से एक शब्द को दूसरे से बदला है। इस तरह से, हम एक सरल और तेज़ तरीके से टेक्स्ट को खोज और बदल सकते हैं।

## गहराई पृष्ठभूमि:

यह तकनीक पहले से ही दोनों वृत्तों के मध्य साथ उस समय अस्थायी रूप से प्रयोग होता था। इसके अलावा, कई अन्य सूची रखे हैं हो सकते हैं। इससे बाहर, यह साधनें छोटे समय में और पूर्ववत रूप से अप्लाइड करने में कुछ दूरी बनाते हैं।

## भी देखें:

- [Stack Overflow पोस्ट](https://stackoverflow.com/questions/37700385/how-do-i-search-and-replace-a-string-in-swift)
- [Apple दस्तावेज़ीकरण](https://developer.apple.com/documentation/foundation/nsstring/1416952-replacingoccurrences)
- [AppCoda ट्यूटोरियल](https://www.appcoda.com/swift-string/)