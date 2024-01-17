---
title:                "मौजूदा तारीख प्राप्त करना"
html_title:           "Swift: मौजूदा तारीख प्राप्त करना"
simple_title:         "मौजूदा तारीख प्राप्त करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?:
"वर्तमान तारीख प्राप्त करना" क्या है, और क्यों प्रोग्रामर्स इसे करते हैं? आमतौर पर, वर्तमान तारीख प्राप्त करने के लिए इस्तेमाल किया जाता है ताकि हम प्रोग्राम को वर्तमान में कहां हैं पता कर सकें और उसके अनुसार हमें कोई कार्रवाई ले सकें।

## कैसे करने के लिए:
```Swift
// मेंत्य कल - शाघार संख्या
let calendar = Calendar.current
let date = Date()
let components = calendar.dateComponents([.day, .month, .year, .weekday], from: date)

print(components.day!) // आज का दिन
print(components.month!) // वर्तमान माह
print(components.year!) // वर्तमान साल
print(components.weekday!) // वर्तमान दिन
```

आप ऊपर दिए गए कोड फ्रेमवर्क का उपयोग करके वर्तमान तारीख को प्राप्त कर सकते हैं। आप तारीख में किसी भी अनुभाग को प्राप्त कर सकते हैं, जैसे कि दिन, माह, साल और हफ्ते का नाम।

## गहराई में जाएं:
वर्तमान तारीख का सब से पहले उपयोग करना इतिहास में प्रारम्भिक प्रोग्रामिंग भाषाओं में से एक की तरह है। आप इसके इलावा भी कई अलग तरीकों से वर्तमान तारीख को प्राप्त कर सकते हैं, जैसे कि अलग-अलग लाइब्रेरी या फ्रेमवर्क का उपयोग करके या खुद कोडिंग करके। इस समस्या को हल करने के लिए Apple ने Swift में Date और Calendar नामक दो डेटा प्रकटिकल्स भी शामिल किए हैं।

## और देखें:
आप वर्तमान तारीख प्राप्त करने के और भी कई तरीके और मेथडों के बारे में जानने के लिए निम्न स्रोतों को देख सकते हैं:

- [Apple डॉक्यूमेंटेशन](https://developer.apple.com/documentation/foundation/calendar)
- [Swift Standard Library गाइड](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID338)

आप इन स्रोतों से साथ में दिए गए कोड फ्रेम्वर्क का भी उपयोग करके प्रोग्रामिंग में वर्तमान तारीख प्राप्त कर सकते हैं। इन स्रोतों की मदद से आप अपने स्वयं के प्रोग्राम्स में वर्तमान तारीख का उपयोग कर सकते हैं और अपने कोड को और बेहतर और उपयोगी बना सकते हैं।