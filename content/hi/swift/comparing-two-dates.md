---
title:                "दो तारीखों की तुलना करना"
html_title:           "Swift: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
कोडिंग में दो तारीखों की तुलना करने से मतलब है कि दो तारीखों के बीच की समय अंतर को पता करना। इसे प्रोग्रामर्स इसलिए करते हैं क्योंकि वे अपने कोड में तारीखों को जाँचने और समय के साथ समझने के लिए इस्तेमाल करते हैं।

## कैसे करें:
`Swift` कोड ब्लॉक के भीतर नमूना उत्पाद और कोडिंग उदाहरण:

`let date1 = Date()     
let date2 = Date()`

इस उदाहरण में, हमने `Date` नामक दो तारीखों को बनाया है। यहाँ `let` बारे में ध्यान दें कि हमने उन्हें बदलाव रोकने के लिए `constant` एबल्। अब, हम `compare()` मेथड का इस्तेमाल करके दो तारीखों के बीच समय अंतर को पता कर सकते हैं:

`let timeInterval = date2.compare(date1)`

## डीप डाइव:
(1) इतिहासिक प्रसंग: पूर्व तारीखों की तुलना को कोडिंग में अत्यधिक उपयोगी बनाने के लिए, `Date` क्लास `Foundation` फ्रेमवर्क से भाग है। यह अन्य `Foundation` क्लासों के साथ तिथि और समय सम्बन्धित काम करता है। (2) वैकल्पिक तरीके: कुछ अन्य तरीके, जैसे कि `calendar` और `time zone` का उपयोग करके भी, हम तारीखों की तुलना कर सकते हैं। (3) अंतर्निहित तत्व: तारीखों को समय के साथ समझने के लिए, हम `Date` वर्ग में `TimeInterval` नामक तत्व का इस्तेमाल करते हैं। यह हमें समय अंतर को मिलीसेकंड में प्रकट करता है।

## देखें भी:
संबंधित स्रोतों के लिंक:  
1) [Apple's Official Documentation on `Date`](https://developer.apple.com/documentation/foundation/date)  
2) [A Beginner Friendly Guide to Understanding Dates in Swift](https://www.raywenderlich.com/965-swift-date-calendar-and-datecomponents)