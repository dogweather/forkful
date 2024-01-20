---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
डेट को स्ट्रिंग से पार्स करना मतलब है डेट को हमारी स्ट्रिंग से अलग करना। कोडर्स इसे तारीखों को कार्यान्वित करने और दृष्टांत करने के लिए करते हैं। 

## कैसे
स्विफ्ट में तारीख को स्ट्रिंग से पार्स करने के लिए `DateFormatter` का उपयोग करें। 

```Swift 
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let date = dateFormatter.date(from: "2022-03-28")

print(date)
```

इसे चलाने पर यह आउटपुट देगा -

```Swift
Optional(2022-03-28 00:00:00 +0000)
```

## विस्तृत जानकारी
स्ट्रिंग से डेट पार्सिंग को iOS 2.0 के लॉन्च के साथ डेट को अधिक सरल और विशेष ज्ञान के बिना प्रक्रिया करने के लिए शामिल किया गया है। इसके विकल्पों में रेगेलर एक्सप्रेशन विधायन शामिल है, लेकिन यह अधिक जटिल हो सकता है। इससे तारीख की तर्जजनक फॉर्मेट को प्रदर्शित करने में सहायता मिलती है। 

## अन्य स्रोतों की जानकारी
निम्नलिखित लिंक में Swift से संबंधित और अधिक जानकारी प्राप्त करें। 
- [Swift Documentation](https://developer.apple.com/documentation/swift)
- [Working with Dates and Time in Swift](https://www.hackingwithswift.com/articles/141/working-with-dates-and-time-in-swift)