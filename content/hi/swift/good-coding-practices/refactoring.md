---
date: 2024-01-26 03:39:04.218886-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u091A\u0932\u093F\
  \u090F \u090F\u0915 \u092C\u0941\u0928\u093F\u092F\u093E\u0926\u0940 Swift \u0909\
  \u0926\u093E\u0939\u0930\u0923 \u0938\u0947 \u0936\u0941\u0930\u0942 \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902 \u091C\u0939\u093E\u0901 \u0939\u092E\u093E\u0930\
  \u0947 \u092A\u093E\u0938 \u0915\u0941\u091B \u092A\u0941\u0928\u0930\u093E\u0935\
  \u0943\u0924\u094D\u0924 \u0915\u094B\u0921 \u0939\u0948."
lastmod: '2024-03-13T22:44:52.932785-06:00'
model: gpt-4-0125-preview
summary: "\u091A\u0932\u093F\u090F \u090F\u0915 \u092C\u0941\u0928\u093F\u092F\u093E\
  \u0926\u0940 Swift \u0909\u0926\u093E\u0939\u0930\u0923 \u0938\u0947 \u0936\u0941\
  \u0930\u0942 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u091C\u0939\u093E\u0901\
  \ \u0939\u092E\u093E\u0930\u0947 \u092A\u093E\u0938 \u0915\u0941\u091B \u092A\u0941\
  \u0928\u0930\u093E\u0935\u0943\u0924\u094D\u0924 \u0915\u094B\u0921 \u0939\u0948\
  ."
title: "\u0915\u094B\u0921 \u0938\u0941\u0927\u093E\u0930"
weight: 19
---

## कैसे करें:
चलिए एक बुनियादी Swift उदाहरण से शुरू करते हैं जहाँ हमारे पास कुछ पुनरावृत्त कोड है:

```Swift
func printUserDetails(firstName: String, lastName: String, age: Int) {
    print("पहला नाम: \(firstName)")
    print("अंतिम नाम: \(lastName)")
    print("उम्र: \(age)")
}

func printUserJob(title: String, company: String) {
    print("नौकरी का शीर्षक: \(title)")
    print("कंपनी: \(company)")
}
```

इसे रिफैक्टर करने में शामिल होगा एक `User` संरचना बनाना जो यूजर विशेषताओं को समेटे और विवरण प्रिंट करने के लिए एक विधि जोड़े:

```Swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
    let jobTitle: String
    let company: String

    func printDetails() {
        print("पहला नाम: \(firstName)")
        print("अंतिम नाम: \(lastName)")
        print("उम्र: \(age)")
        print("नौकरी का शीर्षक: \(jobTitle)")
        print("कंपनी: \(company)")
    }
}

let user = User(firstName: "John", lastName: "Doe", age: 30, jobTitle: "Software Developer", company: "Tech Solutions")
user.printDetails()
```

### नमूना आउटपुट:
```
पहला नाम: John
अंतिम नाम: Doe
उम्र: 30
नौकरी का शीर्षक: Software Developer
कंपनी: Tech Solutions
```

## गहन विचार
रिफैक्टरिंग की जड़ें सॉफ्टवेयर इंजीनियरिंग के शुरूआती दिनों में जाती हैं, लेकिन इस शब्द को 1990 के दशक के अंत में, विशेष रूप से Martin Fowler की महत्वपूर्ण पुस्तक "Refactoring: Improving the Design of Existing Code" के माध्यम से लोकप्रिय बनाया गया। पुस्तक ने सिद्धांत निर्धारित किया कि कोड को छोटे चरणों में लगातार साफ किया जाना चाहिए, बजाय एक अलग चरण की प्रतीक्षा करने के।

मैनुअल रिफैक्टरिंग के विकल्पों में ऑटोमेटेड टूल्स और IDEs (इंटीग्रेटेड डेवलपमेंट एन्वायरमेंट्स) शामिल हैं जो डुप्लिकेट कोड का पता लगा सकते हैं, सरलीकरण का सुझाव दे सकते हैं, और कोड के हिस्सों को ऑटो-जनर�erate कर सकते हैं। Swift डेवलपमेंट के लिए Xcode, विभिन्न रिफैक्टरिंग टूल्स, जैसे की रिनेम और एक्सट्रेक्ट मेथड फंक्शनलिटी, प्रदान करता है, जो प्रक्रिया में मानवीय त्रुटि की संभावना को कम कर सकते हैं।

रिफैक्टरिंग को लागू करते समय, एक मजबूत टेस्ट सूट होना महत्वपूर्ण है। टेस्ट एक सुरक्षा जाल के रूप में काम करते हैं, सुनिश्चित करते हैं कि आपके द्वारा किए जा रहे परिवर्तन कीड़े पेश नहीं कर रहे हैं। यह आवश्यक है क्योंकि रिफैक्टरिंग का मुख्य लक्ष्य बाहरी व्यवहार को प्रभावित किए बिना आंतरिक संरचना को बदलना है।

## देखें भी
- ["रिफैक्टरिंग: एक्सिस्टिंग कोड के डिज़ाईन को बेहतर बनाना" Martin Fowler द्वारा](http://martinfowler.com/books/refactoring.html)
- [Apple द्वारा Swift डॉक्यूमेंटेशन](https://swift.org/documentation/)
- [Xcode रिफैक्टरिंग टूल्स का उपयोग करना](https://help.apple.com/xcode/mac/current/#/dev91fe7130a)
- [Ray Wenderlich की Swift स्टाइल गाइड](https://github.com/raywenderlich/swift-style-guide)
