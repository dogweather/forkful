---
title:                "कोड सुधार"
aliases:
- /hi/swift/refactoring.md
date:                  2024-01-26T03:39:04.218886-07:00
model:                 gpt-4-0125-preview
simple_title:         "कोड सुधार"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/refactoring.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
रिफैक्टरिंग एक्सिस्टिंग कंप्यूटर कोड को उसके बाहरी व्यवहार को बदले बिना पुनर्गठित करने की प्रक्रिया है। प्रोग्रामर इसे कोडबेस को साफ करने, पठनीयता, रखरखाव को बेहतर बनाने, और न्यूनतम तकनीकी ऋण के साथ भविष्य की सुविधाओं के लिए मार्ग प्रशस्त करने के लिए करते हैं।

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
